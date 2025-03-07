# Comment Preservation in LexerV2 (V2 Parser)

## Current State Analysis

The V2 parser currently has basic token representation for comments but doesn't attach them to expressions. Here are the key components:

1. **Token Representation**: 
   - Comments are represented as `Token.Comment(text: String, sourcePos: SourcePos)` tokens during lexing
   - The parser has a `skipComments` method that advances past comments and whitespace without preserving them

2. **Comment Structure in AST**:
   - `chester.syntax.concrete.Comment` class with content, type (OneLine/MultiLine), and sourcePos
   - `chester.syntax.concrete.CommentInfo` class with collections for:
     - commentBefore: Comments before an expression
     - commentInBegin: Comments at the beginning of a block structure
     - commentInEnd: Comments at the end of a block structure
     - commentEndInThisLine: Comments at the end of a line with an expression

3. **Metadata Handling**:
   - `ExprMeta` contains sourcePos and optional commentInfo
   - `MetaFactory` has helper methods for comment attachment
   - Expression nodes have methods like `commentAtStart`, `updateMeta` for comment handling

4. **V1 Parser Approach**:
   - V1 parser collects comments during parsing with methods like `delimiter1` and `lineEnding1`
   - Uses extension methods like `relax` to associate comments with expressions
   - Builds `CommentInfo` structures using `MetaFactory` helpers

## Issues to Solve

1. The V2 parser's `skipComments` method discards comments instead of preserving them
2. No mechanisms to collect and categorize comments (before expressions, end of line, etc.)
3. Missing integration with `ExprMeta` and `CommentInfo` structures
4. Need to handle both leading and trailing comments

## Implementation Plan

### 1. Comment Collection During Lexing/Parsing

Create a new `collectComments` method in `LexerV2` that:
- Takes a LexerState as input
- Returns a tuple of (Vector[chester.syntax.concrete.Comment], LexerState)
- Converts Token.Comment instances to chester.syntax.concrete.Comment objects
- Preserves source positions and comment text
- Categorizes comments based on surrounding whitespace/newlines

```scala
def collectComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
  var comments = Vector.empty[chester.syntax.concrete.Comment]
  var current = state
  
  while (!current.isAtEnd && 
         (current.current.exists(_.isComment) || current.current.exists(_.isWhitespace))) {
    current.current match {
      case Right(Token.Comment(text, sourcePos)) =>
        val comment = chester.syntax.concrete.Comment(
          text, 
          chester.syntax.concrete.CommentType.OneLine, 
          Some(sourcePos)
        )
        comments = comments :+ comment
        current = current.advance()
      case Right(Token.Whitespace(_)) =>
        current = current.advance()
      case _ => // Should never happen due to the while condition
    }
  }
  
  (comments, current)
}
```

### 2. Metadata Creation with Comments

Add methods to create expression metadata with comments:

```scala
def createMetaWithComments(
  sourcePos: Option[SourcePos],
  leadingComments: Vector[chester.syntax.concrete.Comment],
  trailingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty
): Option[ExprMeta] = {
  if (sourcePos.isEmpty && leadingComments.isEmpty && trailingComments.isEmpty) {
    None
  } else {
    val commentInfo = if (leadingComments.isEmpty && trailingComments.isEmpty) {
      None
    } else {
      Some(chester.syntax.concrete.CommentInfo(
        commentBefore = leadingComments,
        commentEndInThisLine = trailingComments
      ))
    }
    Some(ExprMeta(sourcePos, commentInfo))
  }
}
```

### 3. Comment Association with Expressions

Modify the expression creation methods to include comments:

```scala
def parseAtomWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
  // Collect leading comments
  val (leadingComments, afterLeadingComments) = collectComments(state)
  
  // Parse the actual expression
  parseAtom(afterLeadingComments).flatMap { case (expr, afterExpr) =>
    // Collect trailing comments
    val (trailingComments, finalState) = collectTrailingComments(afterExpr)
    
    // Update expression with comments
    val updatedExpr = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
      expr.updateMeta { existingMeta =>
        MetaFactory.add(
          commentBefore = leadingComments,
          commentEndInThisLine = trailingComments
        )(existingMeta)
      }
    } else {
      expr
    }
    
    Right((updatedExpr, finalState))
  }
}
```

### 4. Handling Special Cases

Handle special comment positions:

1. **Block comments**: Comments within blocks should be attached to the block or nearest expression
2. **End-of-line comments**: Comments following an expression on the same line
3. **Standalone comments**: Comments not directly associated with an expression
4. **Comments between expressions**: Determine which expression they belong to

### 5. Integration Points

Update these methods to use comment-aware parsing:
- `parseExpr`
- `parseExprList`
- `parseBlock`
- `parseObject`
- `parseTuple`
- `parseList`

## Testing Strategy

1. Create test cases with various comment placements:
   - Leading comments before expressions
   - Trailing comments after expressions
   - Comments within blocks, lists, and objects
   - Standalone comments between expressions
   
2. Verify comment content and positions are preserved
   
3. Compare output with V1 parser to ensure compatibility
   
4. Test edge cases:
   - Multiple consecutive comments
   - Comments with special characters
   - Comments at file beginning and end

## Compatibility Considerations

Ensure the comment handling is compatible with:
- The V1 parser's comment representation
- Any consumers of the AST that use comment information
- Code formatting tools that rely on comment positions

## Implementation Phasing

1. **Phase 1**: Basic comment collection without categorization
2. **Phase 2**: Comment categorization (leading vs trailing)
3. **Phase 3**: Special case handling (blocks, objects, etc.)
4. **Phase 4**: Test and verification
5. **Phase 5**: Optimization if needed 