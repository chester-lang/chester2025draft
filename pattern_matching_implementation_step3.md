# Pattern Matching Fix Implementation - Step 3: Enable Context for Blocks

## Problem Statement

The V2 parser currently fails to handle pattern matching expressions with blocks after the `=>` operator correctly. In pattern matching, the `}\n` pattern should terminate expressions when they appear after a case clause. In the V1 parser, this behavior is controlled by the `ParsingContext(newLineAfterBlockMeansEnds = true)` flag.

In Steps 1 and 2, we added the context flag to `LexerState` and updated the `checkForRBraceNewlinePattern` method to use this flag. Now we need to actually set this flag to `true` at the appropriate times to enable the correct block termination behavior.

## Current Code Analysis

Currently, the `parseBlock` method in LexerV2 does not set the `newLineAfterBlockMeansEnds` flag:

```scala
def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
  // Replace skipComments with collectComments
  val (_, current) = collectComments(state)
  var statements = Vector[Expr]()
  var result: Option[Expr] = None
  var maxExpressions = 100 // Prevent infinite loops

  // Skip the opening brace
  current.current match {
    case Right(Token.LBrace(sourcePos)) => {
      // Use collectComments instead of skipComments
      val (blockStartComments, afterBrace) = collectComments(current.advance())

      // Create a local state to track comments and expressions
      var blockCurrent = afterBrace

      // Regular block parsing - no special case for "case"
      while (maxExpressions > 0) {
        // ...block parsing logic...
      }
    }
    // ...error handling...
  }
}
```

Similarly, the `parseBlockWithComments` method, which is a wrapper around `parseBlock` that collects comments, does not set this flag.

## Proposed Change

The simplest approach is to enable the `newLineAfterBlockMeansEnds` flag for ALL blocks, as V1 does for pattern matching blocks:

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

We also need to update `parseBlockWithComments` to preserve this context flag:

```scala
def parseBlockWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
  // Collect leading comments
  val (leadingComments, afterLeadingComments) = collectComments(state)
  
  // Parse the block (this will enable newLineAfterBlockMeansEnds context)
  parseBlock(afterLeadingComments).flatMap { case (block, afterBlock) =>
    // ...existing comment attachment logic...
  }
}
```

## Justification

This change:
1. Enables the `}\n` pattern termination for all blocks, which matches V1's behavior for pattern matching
2. Preserves uniform treatment of all tokens (no special handling for `=>` or any operator)
3. Makes the minimal change needed to achieve the desired behavior
4. Follows the V1 parser's approach of using context to determine block termination
5. Maintains the principle of contextual parsing without operator-specific logic

## Implementation Steps

1. Edit `reader/src/main/scala/chester/readerv2/LexerV2.scala`
2. Modify the `parseBlock` method to enable the `newLineAfterBlockMeansEnds` context flag
3. Ensure the context is properly passed through from `parseBlockWithComments` to `parseBlock`

## Testing Plan

Since this change enables the block termination context flag, it should change the behavior of the V2 parser to match the V1 parser for pattern matching. We will:

1. Run all existing tests to verify overall functionality: `sbt rootJVM/test`
2. Specifically run pattern matching tests: `sbt "reader/testOnly chester.reader.PatternMatchingTest"`
3. If tests pass, modify the `match2` test to use `parseAndCheckBoth` instead of `parseAndCheck` to ensure it works with both parsers

With these changes, the `match2` test should now pass with both the V1 and V2 parsers, indicating that we've successfully fixed the pattern matching with blocks issue while maintaining uniform treatment of all operators.
