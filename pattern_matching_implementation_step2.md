# Pattern Matching Fix Implementation - Step 2: Update checkForRBraceNewlinePattern

## Problem Statement

The `checkForRBraceNewlinePattern` method in LexerV2 currently makes decisions based solely on syntactic structure (whether an expression is a block followed by a newline). However, it doesn't consider the parsing context, which is critical for correctly handling pattern matching expressions.

In V1, the `ParsingContext(newLineAfterBlockMeansEnds = true)` context determines whether a `}\n` pattern should terminate an expression. We've added this flag to `LexerState` in Step 1, but we need to update the `checkForRBraceNewlinePattern` method to use this context flag.

## Current Code Analysis

The current implementation of `checkForRBraceNewlinePattern` only checks if:
1. The expression is a Block
2. The current token is whitespace containing a newline

```scala
def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
  expr match {
    case block: Block =>
      state.current match {
        case Right(whitespaceToken: Token.Whitespace) => {
          // Check for newline in whitespace, but doesn't consider context
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

## Proposed Change

Update the `checkForRBraceNewlinePattern` method to consider the `newLineAfterBlockMeansEnds` context flag:

```scala
def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
  // Only consider }\n as terminating if we're in the right context
  if (!state.newLineAfterBlockMeansEnds) return false
  
  // Rest of existing implementation
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

## Justification

This change:
1. Makes the `}\n` pattern handling context-aware
2. Preserves the existing logic for detecting newlines after blocks
3. Maintains uniform treatment of all tokens (no special handling for specific operators)
4. Keeps behavior unchanged when context flag is false (current default)
5. Follows the V1 parser's approach of using context to determine expression termination

## Implementation Steps

1. Edit `reader/src/main/scala/chester/readerv2/LexerV2.scala`
2. Add the context check at the beginning of `checkForRBraceNewlinePattern`
3. Leave the rest of the method implementation unchanged

## Testing Plan

Since this change only adds conditional logic with a default value matching the current behavior, no behavior changes are expected yet. We will:

1. Run all existing tests to verify nothing broke: `sbt rootJVM/test`
2. Specifically run pattern matching tests: `sbt "reader/testOnly chester.reader.PatternMatchingTest"`

The tests should have the same results as before this change (match test passing, match2 test still failing with V1 parser).
