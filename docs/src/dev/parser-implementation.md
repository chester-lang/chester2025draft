# Chester Parser Implementation Details

This document covers implementation details and optimization techniques used in the Chester parser (readerv2). While [parser-plan.md](parser-plan.md) focuses on the high-level architecture and migration plan, this document dives into specific implementation strategies and optimizations.

## Tokenizer Optimizations

### UTF-16 Position Caching

The Tokenizer needs to maintain a mapping between byte positions and UTF-16 character positions for proper source mapping and error reporting. This is particularly important for Unicode characters that can span multiple bytes.

#### Implementation Strategy

We use a caching mechanism to avoid recalculating UTF-16 positions for the same byte positions. The implementation includes:

1. **Efficient Cache Lookup**:
   ```scala
   private def getUtf16Position(bytePos: Int): Int = {
     utf16PosCache.computeIfAbsent(bytePos, pos => {
       // Find the nearest cached position that's less than or equal to the target position
       val entry = utf16PosCache.entrySet().stream()
         .filter(e => e.getKey() <= bytePos)
         .max((a, b) => a.getKey().compareTo(b.getKey()))
         .orElse(new java.util.AbstractMap.SimpleEntry(0, 0))
       
       val nearestPos = entry.getKey()
       val nearestUtf16 = entry.getValue()
       
       // Calculate incrementally from nearest cached position
       nearestUtf16 + (if (nearestPos == bytePos) 0
                      else source.substring(nearestPos, bytePos).getCodePoints.size)
     })
   }
   ```

2. **Streamlined Position Updates**:
   ```scala
   private def updatePosition(newPos: Int): Unit = {
     if (newPos > pos) {
       getUtf16Position(newPos) // This will compute and cache if needed
     }
     pos = newPos
   }
   ```

#### Optimization Benefits

- **Lazy Calculation**: Positions are only calculated when needed
- **Incremental Computation**: Uses the nearest cached position as a starting point
- **Reduced Code Duplication**: Cache handling logic is centralized in one method
- **More Maintainable Code**: Cleaner, more functional approach using Java streams API

## LexerV2 Optimizations

### Token Terminator Detection

Detecting terminator tokens (like `)`, `}`, `]`, `,`, `;`, and EOF) is a common operation in the parser. We've optimized this pattern by:

1. **Centralizing Terminator Logic**:
   ```scala
   private def isTerminator(token: Token): Boolean = token match {
     case _: Token.RParen | _: Token.RBrace | _: Token.RBracket | _: Token.Comma | _: Token.Semicolon | _: Token.EOF => true
     case _ => false
   }

   private def isAtTerminator(state: LexerState): Boolean = state.current match {
     case Right(token) => isTerminator(token)
     case _ => false
   }
   ```

2. **Cleaner Pattern Matching**:
   ```scala
   // Before:
   current.current match {
     case Right(Token.EOF(_)) | Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) | 
          Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
       // Handle terminator
     }
     // Other cases...
   }

   // After:
   if (isAtTerminator(current)) {
     // Handle terminator
     return buildOpSeq(localTerms).map(result => (result, current))
   }
   ```

### MetaData Creation

The `createMeta` method is used throughout the parser to attach source position information to syntax nodes. We optimized it with clearer pattern matching:

```scala
private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] = {
  if (ignoreLocation) return None
  
  (startPos, endPos) match {
    case (Some(start), Some(end)) => 
      Some(ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None))
    case (Some(pos), None) => 
      Some(ExprMeta(Some(pos), None))
    case (None, Some(pos)) => 
      Some(ExprMeta(Some(pos), None))
    case _ => 
      None
  }
}
```

### Comment Handling

Comment handling in the parser is critical for preserving code formatting and documentation. Key aspects:

1. **Consistent Comment Collection**: 
   - Comments are collected using the `collectComments` method
   - This pattern is applied uniformly across different parsing contexts

2. **Proper Comment Association**:
   - Comments are associated with the appropriate expressions
   - Both leading and trailing comments are preserved

## Design Patterns

### Pattern Matching for Type Dispatch

Scala's pattern matching is extensively used to handle different token types. This approach:

1. Makes the code more readable
2. Ensures exhaustive handling of all cases
3. Centralizes related logic

### Early Returns

We use early returns in complex parsing functions to improve code clarity and reduce nesting:

```scala
// Handle terminators early
if (isAtTerminator(current)) {
  debug("parseRest: Hit terminator token")
  return buildOpSeq(localTerms).map(result => (result, current))
}

// Continue with other cases
current.current match {
  // ...
}
```

## Pattern Matching Block Termination Fix

The V2 parser faced a specific challenge with pattern matching expressions containing blocks after the `=>` operator. This section details the implementation approach for fixing this issue while maintaining uniform symbol treatment.

### Block Termination Context Implementation

A key insight in fixing pattern matching was understanding how the V1 parser uses context to determine when a `}\n` pattern should terminate an expression. In V1, this is handled via `ParsingContext(newLineAfterBlockMeansEnds = true)`, while V2 needed a similar mechanism.

#### 1. Adding Context to LexerState

We added a boolean flag to track when newlines after blocks should terminate expressions:

```scala
case class LexerState(
    tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    newLineAfterBlockMeansEnds: Boolean = false  // NEW FIELD
) {
  // Existing methods
  
  // Helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState = 
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)
}
```

This approach:
- Adds minimal context tracking to the parser
- Uses immutable state modification via the helper method
- Maintains the default behavior (false) for backward compatibility

#### 2. Context-Aware Block Termination

The `checkForRBraceNewlinePattern` method was updated to consider the context flag:

```scala
def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
  // Only consider }\n as terminating if we're in the right context
  if (!state.newLineAfterBlockMeansEnds) return false
  
  expr match {
    case block: Block =>
      state.current match {
        case Right(whitespaceToken: Token.Whitespace) => {
          // Existing check for newline in whitespace
          val startPos = whitespaceToken.sourcePos.range.start.index.utf16
          val endPos = whitespaceToken.sourcePos.range.end.index.utf16
          val maybeSource = sourceOffset.readContent.toOption
          maybeSource.exists { source =>
            if (startPos < source.length && endPos <= source.length) {
              val whitespaceText = source.substring(startPos, endPos)
              whitespaceText.contains('\n')
            } else false
          }
        }
        case _ => false
      }
    case _ => false
  }
}
```

This change preserves the existing logic for detecting newlines after blocks while adding context awareness.

#### 3. Enabling Context for All Blocks

Finally, we modified the `parseBlock` method to enable the context flag for all blocks:

```scala
def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
  // IMPORTANT: Enable newLineAfterBlockMeansEnds for all blocks
  // This preserves uniform treatment without special-casing any operators
  val contextState = state.withNewLineTermination(true)
  
  // Replace skipComments with collectComments
  val (_, current) = collectComments(contextState)
  
  // Rest of existing implementation, using current
  // ...
}
```

### Uniform Symbol Treatment Preservation

A critical aspect of this fix was preserving Chester's uniform symbol treatment principle. No special handling was added for the `=>` operator or any other identifiers:

- The context affects only how block termination works, not how specific operators are treated
- All operators continue to be parsed as plain identifiers without any semantic meaning
- The `=>` token is treated exactly like any other identifier in the parser

The implementation details show how we can maintain context-free parsing principles while still handling contextual significance of newlines after blocks. For implementation status and next steps, see the devlog entry for 2025-03-22.

## Future Optimization Opportunities

1. **Further Token Stream Optimizations**:
   - Consider token stream buffering techniques
   - Explore specialized token handling for common cases

2. **Error Recovery Enhancements**:
   - Implement more sophisticated recovery strategies
   - Improve error message readability

3. **Memory Usage Optimization**:
   - Evaluate and reduce memory footprint where possible
   - Consider pooling common token instances
