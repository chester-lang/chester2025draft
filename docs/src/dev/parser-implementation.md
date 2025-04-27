# Chester Parser Implementation Details

This document covers key implementation strategies in the Chester V2 parser. 
Refer to `docs/src/dev/devlog.md` for a chronological log of recent improvements.

## Core Implementation Details

### Tokenizer Optimizations
- **UTF-16 Position Handling**: Simplified tracking with proper UTF-16 coordinate mapping, supporting surrogate pairs.
- **Token Creation**: Centralized creation with lookup tables and specialized handlers. Extractors use common helpers.
- **Unicode/Emoji**: Unicode-aware codepoint handling.

### State Management
- `LexerState` tracks context (e.g., for block termination) and handles pending tokens, comments, and whitespace.
- **NEW** `peek` method added to `LexerState` for lookahead without consuming tokens.
- **NEW** `withModifiedState` helper simplifies state manipulation during parsing actions.

### Comment Handling
- Optimized collection (`skipComments()`, `pullComments()`) and attachment to expressions.
- Metadata merging preserves comment information.

### Expression Parsing (Flat OpSeq)
- **Key Principle**: Chester V2 parser **intentionally** produces flat `OpSeq` nodes without handling operator precedence. This is done later by the semantic analyzer.
- Uniform symbol treatment; no special keywords.

### Block Termination (`}\n`)
- The `}\n` pattern terminates expressions within blocks.
- `LexerState` tracks context (`newLineAfterBlockMeansEnds` flag).
- `parseRest()` detects the pattern based on whitespace analysis.

### Object Expressions
- Supports identifier, string literal, and symbol literal keys.
- Handles both `=` and `=>` operators.
- Parses comma-separated clauses.

### Function Calls & Block Arguments
- Context-aware parsing handles blocks used as function arguments.

### String and Number Parsing
- Handles various formats (decimal, hex, binary, float) and escape sequences.

## Recent Refinements (See `devlog.md` for details)

- **`LexerV2` Refactoring**: `parseAtom` simplified, `peek` added to `LexerState`, `withModifiedState` introduced.
- **Completed Features**: Object expressions, block termination, and semantic consistency with V1 are fully implemented and verified.

## Future Work (See `parser-plan.md` Phase 3)

- Error recovery enhancements.
- Source Maps support.
- Migration of remaining V1 tests.
- Performance optimization (token stream buffering, memory usage).
