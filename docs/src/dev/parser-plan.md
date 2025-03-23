# Chester Parser Architecture and Improvement Plan

## Overview

Chester is migrating from the original `reader` implementation (V1) to a new `readerv2` (V2). This document outlines the migration plan and pending improvements.

## Parser Development Guidelines

### Core Principles

1. **Maintain Context-Free Parsing**
   - Prefer context-free over context-dependent parsing
   - Avoid parser decisions based on semantic meaning
   - Use uniform rules for terminating expressions
   - Treat all identifiers uniformly

2. **Separation of Concerns**
   - Separate parsing (syntax) from semantic analysis
   - Handle operator precedence, fixity, and associativity in later passes
   - Produce flat OpSeq nodes without interpreting operator semantics

3. **Verification Practices**
   - Test both parsers using `parseAndCheckBoth` where possible
   - Ensure no special handling for specific identifiers

## Parser Architecture

### Syntax Design Principles

1. **Uniform Symbol Treatment**
   - All identifiers and operators treated uniformly during parsing
   - No special cases for keywords - they're just identifiers

2. **Operator and Identifier Rules**
   - Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
   - Identifiers start with letters/emoji/underscore
   - Both can contain operator symbols and word symbols

3. **Space and Newline Handling**
   - Spaces significant in specific contexts (function calls, operator sequences)
   - Newlines significant after blocks (i.e., `}\n` terminates expressions)

4. **Block Return Value Semantics**
   - Last expression in block is the return value
   - Trailing semicolon returns unit

### Parser Implementations

- **V1**: FastParse combinators for declarative style
- **V2**: Token-based parsing with state machine approach

## Migration Status

### Feature Comparison

| Feature | Reader (V1) | ReaderV2 (V2) | Notes |
|---------|-------------|---------------|-------|
| Basic Literals | ✅ | ✅ | Integers, floating-point numbers supported |
| Function Calls | ✅ | ✅ | Full support in V2 |
| Pattern Matching | ✅ | ✅ | Now supports uniform treatment |
| Object Syntax | ✅ | 🟡 | Basic support in V2 |
| Operator Sequence | ✅ | ✅ | Parser produces flat OpSeq nodes |
| Error Recovery | ✅ | 🟡 | Comment preservation implemented |
| Generic Type Parameters | ✅ | ✅ | Full support |
| Block Arguments | ✅ | ✅ | Properly supported |
| Comment Preservation | ✅ | ✅ | Fully supported |

Legend:
- ✅ Fully Implemented
- 🟡 Partially Implemented
- 🔴 Not Yet Implemented

## Implementation Plan

### Phase 1: Core Functionality (✅ Complete)
- Basic literals, function calls, operators, blocks
- Lists with mixed types
- Generic type parameters
- Comment preservation

### Phase 2: Advanced Features (🟡 Current)
- 🟡 Object expressions with string literal and symbol keys
- 🔴 Complex object syntax
- 🔴 Source maps

### Phase 3: Error Handling (🔴 Planned)
- 🔴 Error recovery
- 🔴 Improved error messages
- 🔴 Source position tracking
- 🔴 Debug information

## Current Priorities

### 1. V1/V2 Semantic Consistency
- Ensure both parsers produce the same AST for identical inputs
- Focus on remaining differences in complex operator sequences
- Address pattern matching semantic differences

### 2. Object Expressions
- Complete object expressions implementation with complex syntax support

### 3. Block Termination and Newline Handling
- Address the `}\n` pattern handling for expression termination

## Next Steps

1. Complete object expressions implementation
2. Add source maps support
3. Implement error recovery mechanisms
4. Continue migration of all remaining V1-only tests to V2