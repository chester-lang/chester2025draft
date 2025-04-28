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

ReaderV1 uses a hierarchical structure of parser components:

1. **Expression Parsers**: Methods like `parseExpr`, `parseAtom`, and `parseOperator` form the core of the parser. They use FastParse combinators to build complex parsers from simpler ones.

2. **Context Tracking**: A `ParsingContext` object tracks the current parsing state, including whether we're in an operator sequence, a block, or other specialized contexts.

3. **Source Position Tracking**: Dedicated methods map character positions to line/column positions for error reporting, with special handling for UTF-16 surrogate pairs.

4. **Whitespace and Comment Handling**: Dedicated parsers for whitespace, line endings, and comments ensure these elements are preserved in the AST.

5. **Parser Extensions**: Custom extension methods for FastParse parsers add support for metadata attachment, relaxed parsing, and error recovery.

6. **Parser Composition**: The implementation composes smaller parsers into larger ones, following FastParse's combinator approach.

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

ReaderV2 implements a token-based parsing approach:

1. **Two-Phase Parsing**: Separates tokenization from parsing, with a dedicated Tokenizer creating a stream of tokens before parsing begins.

2. **State Machine**: The parser maintains explicit state through a `ReaderState` object that tracks the current token, previous tokens, pending comments/whitespace, and other contextual information.

3. **Context-Aware Processing**: A `ReaderContext` object tracks semantic context, enabling context-sensitive decisions during parsing.

4. **Explicit Comment Handling**: Methods like `skipComments()` and `pullComments()` manage comment attachment without recursive descent, improving performance.

5. **Block Termination Detection**: The special `}\n` pattern detection uses explicit context tracking and lookahead to determine when blocks end.

6. **Error Handling**: The parser produces structured `ParseError` objects with detailed source position information and recovery mechanisms.

7. **Bottom-Up Construction**: Parsing builds expressions from atoms and then extends them through continuation-based parsing in `parseRest()`.

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
