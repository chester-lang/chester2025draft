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
| Object Syntax | ✅ | ✅ | Basic support with multiple key types |
| Operator Sequence | ✅ | ✅ | Flat OpSeq nodes |
| Token Extractors | ✅ | ✅ | Simplified with common helpers |
| Error Recovery | ✅ | 🟡 | Comment preservation done; recovery pending |
| Generic Types | ✅ | ✅ | Complete support |
| Block Arguments | ✅ | ✅ | Properly supported |
| Comment Preservation | ✅ | ✅ | Leading and trailing comments |
| Block Termination | ✅ | ✅ | Context-aware `}\n` handling |
| Unicode & Emoji | ✅ | ✅ | Robust UTF-16 handling |

## Implementation Phases

### Phase 1: Core Functionality (✅ Complete)
- Basic literals, functions, operators, blocks, lists
- Generic type parameters, comments, Unicode support

### Phase 2: Advanced Features (✅ Current)
- ✅ Basic object expressions
- 🟡 Complex object syntax
- 🔴 Source maps

### Phase 3: Error Handling (🔴 Planned)
- 🔴 Error recovery
- 🔴 Improved error messages
- 🟡 Source position tracking
- 🔴 Debug information

## Current Priorities

1. **V1/V2 Semantic Consistency** (✅ Complete)
   - Both parsers produce same AST
   - Pattern matching differences resolved
   - Block termination handling aligned

2. **Object Expressions** (🟡 In Progress)
   - ✅ Basic object expressions
   - 🟡 Complex object syntax with nested expressions

3. **Block Termination** (✅ Complete)
   - Block termination with `}\n` pattern
   - Context tracking implementation

## Next Steps
1. Complete complex object expressions
2. Add source maps support
3. Implement error recovery
4. Migrate remaining V1-only tests
5. Expand test coverage