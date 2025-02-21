# Parser Migration Status

## Overview

Chester is currently undergoing a parser migration from the original `reader` implementation to a new `readerv2`. This document tracks the status of this migration and outlines the goals and progress.

## Current Status

### ReaderV2 Implementation
- Currently passes a subset of tests
- Implements core parsing functionality
- Work in progress to achieve feature parity with original reader

### Original Reader (Legacy)
- Fully functional and used in production
- Serves as reference implementation
- Will be maintained until ReaderV2 reaches feature parity

## Feature Comparison

| Feature | Reader (Original) | ReaderV2 | Notes |
|---------|------------------|----------|--------|
| Basic Literals | ✅ | ✅ | Integers, strings, etc. |
| Function Calls | ✅ | 🟡 | Basic support in V2 |
| Pattern Matching | ✅ | 🔴 | Not yet implemented in V2 |
| Object Syntax | ✅ | 🟡 | Basic support in V2 |
| Operator Sequence | ✅ | ✅ | Parser produces flat OpSeq nodes |
| Error Recovery | ✅ | 🔴 | Planned for V2 |
| Source Maps | ✅ | 🔴 | To be implemented |
| Unicode Support | ✅ | ✅ | Full support in both |

Legend:
- ✅ Fully Implemented
- 🟡 Partially Implemented
- 🔴 Not Yet Implemented

## Migration Goals

1. **Feature Parity**
   - Match all functionality of original reader
   - Pass all existing test cases
   - Maintain same error reporting quality

2. **Improvements**
   - Better error recovery
   - More maintainable code structure
   - Better performance
   - Clearer separation of concerns

3. **Documentation**
   - Better documented parsing rules
   - Clear error message guidelines
   - Migration guide for contributors

## Test Coverage

### Currently Passing Tests
```scala
// Basic parsing tests
parse valid identifier
parse identifier with symbols
parse empty input
parse valid decimal integer
parse valid hexadecimal integer
parse valid binary integer
parse valid double with exponent
parse double without exponent
parse integerLiteral
parse doubleLiteral
parse single-line string literal
parse escaped characters in string literal

// Function and block tests
parse simple function call with no arguments
parse function call with multiple arguments
parse block with multiple statements
```

### Tests Still Needed
- Complex pattern matching
- Error recovery scenarios
- Edge cases in object syntax
- Source position tracking

## Implementation Plan

1. **Phase 1: Core Functionality** (Current)
   - [x] Basic literal parsing
   - [x] Simple function calls
   - [x] Basic operator sequence parsing (flat OpSeq nodes)
   - [ ] Pattern matching basics

2. **Phase 2: Advanced Features**
   - [ ] Full pattern matching
   - [ ] Complex object syntax
   - [ ] Source maps

3. **Phase 3: Error Handling**
   - [ ] Error recovery
   - [ ] Improved error messages
   - [ ] Source position tracking
   - [ ] Debug information

## Contributing

When working on the parser migration:

1. **Adding Features**
   - Check feature status in comparison table
   - Add tests matching original reader behavior
   - Document any deviations or improvements

2. **Testing**
   - Run both parser implementations
   - Compare outputs for identical inputs
   - Document any differences

3. **Documentation**
   - Update this status document
   - Add parsing rules documentation
   - Document error messages

## Important Design Principles

1. **Separation of Concerns**
   - Parser only produces flat OpSeq nodes without any knowledge of operator semantics
   - Operator precedence, fixity (infix/prefix/postfix), and associativity are handled in later passes
   - This separation allows flexible operator definition and extension

2. **Error Handling**
   - Designed to handle incomplete/broken source code
   - Produces meaningful partial results when possible
   - Uses ErrorExpr to represent recoverable parse errors

3. **Incremental Parsing**
   - Supports partial parsing of incomplete expressions
   - Maintains parser state for potential incremental updates
   - Useful for IDE integration

## Next Steps

1. **Immediate Tasks**
   - Complete pattern matching support
   - Improve function call parsing
   - Enhance error reporting

2. **Future Work**
   - Implement error recovery
   - Add source maps support
   - Complete advanced features

3. **Documentation**
   - Document new parser architecture
   - Create migration guide
   - Update test documentation 