# Chester Parser Architecture and Improvement Plan

## Overview
Chester is migrating from the original `reader` (V1) to `readerv2` (V2).

## Core Principles
1. **Context-Free Parsing**: Uniform rules for expressions; identifiers treated uniformly
2. **Separation of Concerns**: Parse syntax without imposing semantics
3. **Verification**: Test both parsers with `parseAndCheckBoth`

## Parser Architecture

### Syntax Design
- **Uniform Symbol Treatment**: No special keywords - just identifiers
- **Operator Rules**: Operators start with symbols, identifiers with letters/emoji/underscore
- **Newline Significance**: `}\n` terminates expressions in blocks
- **Block Return Values**: Last expression is return value; trailing semicolon returns unit

### Implementations
- **V1**: FastParse combinators
- **V2**: Token-based state machine approach

## Feature Status

| Feature | V1 | V2 | Notes |
|---------|-------------|---------------|-------|
| Basic Literals | ✅ | ✅ | Integers, floats, binary, hex |
| Function Calls | ✅ | ✅ | Complete support |
| Pattern Matching | ✅ | ✅ | Uniform symbol treatment |
| Object Syntax | ✅ | ✅ | Support for identifier, string, and symbol keys |
| Operator Sequence | ✅ | ✅ | **⚠️ IMPORTANT**: Flat OpSeq nodes without precedence handling (intentional design) |
| Token Extractors | ✅ | ✅ | Simplified with common helpers |
| Error Recovery | ✅ | 🟡 | Comment preservation done; recovery pending |
| Generic Types | ✅ | ✅ | Complete support |
| Block Arguments | ✅ | ✅ | Properly supported |
| Comment Preservation | ✅ | ✅ | Leading and trailing comments |
| Block Termination | ✅ | ✅ | Context-aware `}\n` handling |
| Unicode & Emoji | ✅ | ✅ | Robust UTF-16 handling |
| Dot Notation | ✅ | ✅ | Method call syntax |
| Whitespace Handling | ✅ | ✅ | Consistent handling with newline awareness |

## Current Priorities

1. **V1/V2 Semantic Consistency** (✅ Complete)
   - Both parsers produce same AST
   - Pattern matching differences resolved
   - Block termination handling aligned

2. **Object Expressions** (✅ Complete)
   - ✅ Basic object expressions
   - ✅ Support for identifier, string, and symbol literal keys
   - ✅ Support for both `=` and `=>` operators in object clauses

3. **Block Termination** (✅ Complete)
   - ✅ Block termination with `}\n` pattern
   - ✅ Context tracking implementation
   - ✅ Proper handling of whitespace with newlines

## Next Steps
1. Complete error recovery implementation
2. Add source maps support
3. Migrate remaining V1-only tests
4. Expand test coverage
5. Optimize token handling for better performance