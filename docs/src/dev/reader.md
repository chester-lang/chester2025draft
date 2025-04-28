# Chester Reader Architecture

This document describes the core design of Chester's parsers (also called "readers"), which transform Chester source code into abstract syntax trees (ASTs).

## Overview

Chester currently has two parser implementations:

1. **ReaderV1**: The original parser using FastParse combinators
2. **ReaderV2**: The newer implementation using a token-based state machine

Both parsers adhere to the same core principles and produce semantically identical ASTs, though they use different internal approaches.

## Core Design Principles

1. **Context-Free Parsing**: Uniform rules for all expressions; identifiers treated consistently
2. **Separation of Concerns**: Parse syntax without imposing semantics 
3. **Uniform Symbol Treatment**: No special keywords - just identifiers and operators
4. **Flat Operator Sequences**: Operator precedence handled later in the semantic phase
5. **Newline Significance**: `}\n` terminates expressions in blocks
6. **Block Return Values**: Last expression in a block is its return value

## ReaderV1 Implementation

ReaderV1 uses the FastParse library to implement a parser combinator approach.

### Key Components

- **TokenParsers**: Small parsers for basic lexemes (identifiers, literals, operators)
- **Combinators**: Composable functions that build larger parsers from smaller ones
- **ParsingContext**: Tracks parsing state (e.g., whether currently in an operator sequence)
- **ExprMeta**: Metadata handling for source positions and comments

### Strengths

- Declarative approach with readable grammar definitions
- Good error reporting with FastParse diagnostics
- Mature and stable implementation
- Recursive descent makes some patterns easier to express

### Implementation Details

```scala
case class ReaderV1(
    sourceOffset: Source,
    ignoreLocation: Boolean = false,
    defaultIndexer: Option[StringIndex] = None
)(using p: P[?]) {
  // Parser state
  private def nEnd: P[Unit] = P("\n" | End)
  private def commentOneLine: P[Comment] = ...
  private def delimiter1: P[Vector[Comment]] = ...
  
  // Parse expressions 
  def parseExpr(ctx: ParsingContext = ParsingContext()): P[Expr] = ...
  
  // Helper parsers for different expression types
  def parseOperator(ctx: ParsingContext): P[Expr] = ...
  def parseAtom(ctx: ParsingContext): P[Expr] = ...
  def parseBlock(ctx: ParsingContext): P[Block] = ...
  
  // Track source positions for error reporting
  private def loc(begin: Natural, end: Natural): Option[SourcePos] = ...
}
```

## ReaderV2 Implementation

ReaderV2 uses a custom tokenizer and a state machine-based approach for parsing.

### Key Components

- **Tokenizer**: Converts source code into a stream of tokens
- **ReaderState**: Tracks current position, token history, and pending tokens
- **ReaderContext**: Tracks semantic context for parsing decisions
- **Token Handlers**: Specialized methods for parsing different token types

### Strengths

- More efficient processing with pre-tokenization
- Cleaner separation of lexing and parsing phases
- Better context tracking for complex syntactic patterns
- Improved Unicode and emoji handling
- More maintainable state management

### Implementation Details

```scala
class ReaderV2(initState: ReaderState, source: Source, ignoreLocation: Boolean) {
  // State management
  var state: ReaderState = initState
  
  // Main parsing methods
  def parseExpr(context: ReaderContext = ReaderContext()): Either[ParseError, Expr] = ...
  private def parseAtom(context: ReaderContext): Either[ParseError, Expr] = ...
  private def parseRest(expr: Expr, context: ReaderContext): Either[ParseError, Expr] = ...
  
  // Helper methods for common parsing patterns
  private def checkForRBraceNewlinePattern(context: ReaderContext): Boolean = ...
  private def skipComments(): Unit = ...
  private def pullComments(): Vector[Comment] = ...
  
  // Specialized parsers for complex structures
  private def parseTuple(context: ReaderContext): Either[ParseError, Tuple] = ...
  private def parseObject(context: ReaderContext): Either[ParseError, ObjectExpr] = ...
  private def parseBlock(context: ReaderContext): Either[ParseError, Block] = ...
}
```

## Key Similarities Between Implementations

Both parsers:

1. Track source positions for error reporting
2. Preserve comments in the AST
3. Handle the `}\n` block termination pattern
4. Produce flat operator sequences without precedence handling
5. Parse the same language syntax
6. Use context tracking for parsing decisions
7. Generate identical AST structures

## Key Differences Between Implementations

| Feature | ReaderV1 | ReaderV2 |
|---------|----------|----------|
| **Parsing Approach** | Parser combinators (FastParse) | Token-based state machine |
| **Error Recovery** | Limited | Enhanced with token-based recovery |
| **Token Creation** | On-demand during parsing | Separate tokenization phase |
| **State Handling** | Implicit in parse context | Explicit in ReaderState |
| **Code Structure** | Grammar-centric | Process-centric |
| **Performance** | Good | Better (especially on large files) |
| **Unicode Support** | Basic | Enhanced with better UTF-16 handling |

## Testing and Verification

Both parsers are extensively tested through:

1. **Unit Tests**: Individual parsers for expressions, blocks, etc.
2. **Integration Tests**: Full file parsing with AST verification
3. **Property Tests**: Randomized inputs to verify robustness
4. **Cross-Verification**: `parseAndCheckBoth` ensures both parsers produce identical results

## Future Development

ReaderV2 is the focus of ongoing development, with priorities including:

1. Completing error recovery implementation
2. Adding source maps support
3. Migrating any remaining V1-only tests
4. Expanding test coverage
5. Optimizing token handling for better performance

For a detailed chronological record of parser improvements, see [devlog.md](devlog.md).
