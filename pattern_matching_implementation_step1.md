# Pattern Matching Fix Implementation - Step 1: Add Context to LexerState

## Problem Statement

The V2 parser fails to correctly handle pattern matching expressions with blocks after the `=>` operator because it lacks the contextual tracking needed to properly terminate expressions when encountering the `}\n` pattern. This contextual tracking exists in the V1 parser through the `ParsingContext(newLineAfterBlockMeansEnds = true)` parameter.

## Current Code Analysis

In the current V2 implementation:

1. `LexerState` class tracks the token stream, current position, and previous token:
   ```scala
   case class LexerState(
       tokens: Vector[Either[ParseError, Token]],
       index: Int,
       previousToken: Option[Token] = None
   ) {
     // Method implementations
   }
   ```

2. The `checkForRBraceNewlinePattern` method checks for newlines after blocks but has no way to know when this pattern should terminate an expression:
   ```scala
   def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
     expr match {
       case block: Block =>
         state.current match {
           case Right(whitespaceToken: Token.Whitespace) => {
             // Check for newline in whitespace
             // But no context awareness for whether this pattern matters
           }
           case _ => false
         }
       case _ => false
     }
   }
   ```

## Proposed Change

Add a boolean flag to `LexerState` to track if we're in a context where newlines after blocks should terminate expressions:

```scala
case class LexerState(
    tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    newLineAfterBlockMeansEnds: Boolean = false  // NEW FIELD
) {
  // Existing methods
  
  // Add helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState = 
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)
}
```

## Justification

This change:
1. Adds minimal context tracking to the parser
2. Follows the V1 parser's approach of using a boolean flag to determine expression termination behavior
3. Maintains the uniform treatment of all tokens (no special handling for `=>` or any operator)
4. Makes no behavioral changes yet (default is `false`)
5. Uses immutable state modification with the helper method `withNewLineTermination`

## Implementation Steps

1. Edit `reader/src/main/scala/chester/readerv2/LexerV2.scala`
2. Add the `newLineAfterBlockMeansEnds` field to `LexerState` with a default value of `false`
3. Add the `withNewLineTermination` helper method to the `LexerState` class
4. Leave all existing code behavior unchanged

## Testing Plan

Since this change only adds a field with a default value matching the current behavior, no behavior changes are expected. We will:

1. Run all existing tests to verify nothing broke: `sbt rootJVM/test`
2. Specifically run pattern matching tests: `sbt "rootJVM/testOnly chester.reader.PatternMatchingTest"`

The tests should have the same results as before this change (match test passing, match2 test still failing).
