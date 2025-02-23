# Parser Migration Status

## Overview

Chester is currently undergoing a parser migration from the original `reader` implementation to a new `readerv2`. This document tracks the status of this migration and outlines the goals and progress.

## Design Evolution

### Uniform Symbol Treatment
- All identifiers and operators are treated uniformly in parsing
- NO special cases for keywords like "case", "if", "then", "else" - they are just identifiers
- Parser doesn't distinguish between keywords and regular identifiers
- Semantic meaning determined in later passes
- Examples:
  ```scala
  // These parse exactly the same way:
  case x => y
  myCase x => y
  
  // Both produce:
  OpSeq([identifier, expr, identifier, expr])
  
  // Pattern matching is just a sequence of identifiers and expressions
  x match {
    case A => expr1;
    case B => expr2;
  }
  // Parses as regular identifiers and operators:
  OpSeq([
    identifier("x"),
    identifier("match"),
    Block([
      OpSeq([identifier("case"), identifier("A"), identifier("=>"), expr1]),
      OpSeq([identifier("case"), identifier("B"), identifier("=>"), expr2])
    ])
  ])
  ```
- Benefits:
  - Simpler, more maintainable parser
  - Allows user-defined keywords and operators
  - Consistent parsing rules
  - Flexible operator definition
  - Single source of truth for identifier rules
  - Pattern matching is just regular operator sequences
  - No special parsing rules needed for any construct

### Original Parser Design (V1)
- Parser treats all symbols uniformly from the start
- No special cases or keyword recognition
- All constructs parsed as operator sequences
- Pattern matching, if/then/else, and other constructs are just identifier sequences
- Semantic analysis happens in later passes
- This design proved highly maintainable and flexible

### Implementation Journey
- Currently implemented with special cases in ReaderV2:
  - Special handling for "case" keyword
  - Pattern matching treated as special syntax
  - Keywords recognized during parsing
- Planning to move to generalized approach matching original reader:
  - Remove keyword recognition in parser
  - Implement uniform treatment of all identifiers
  - Handle pattern matching through regular operator sequence parsing
  - Defer semantic analysis to later passes
- Lessons from V1:
  - Special cases increase complexity
  - Uniform treatment simplifies implementation
  - Separating parsing from semantics is powerful
  - Original design's approach is better

## Current Status

### ReaderV2 Implementation
- Currently passes a subset of tests
- Implements core parsing functionality with some special cases
- Work in progress to achieve feature parity with original reader
- Need to remove special case handling for pattern matching

### Original Reader (Legacy)
- Fully functional and used in production
- Serves as reference implementation
- Will be maintained until ReaderV2 reaches feature parity

## Feature Comparison

| Feature | Reader (Original) | ReaderV2 | Notes |
|---------|------------------|----------|--------|
| Basic Literals | ✅ | ✅ | Integers, strings, etc. |
| Function Calls | ✅ | ✅ | Full support in V2 |
| Pattern Matching | ✅ | ✅ | Now supports uniform treatment |
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

### Test Implementation Status

| Test File | V1 Only | Both V1 & V2 | Notes |
|-----------|---------|--------------|--------|
| OpSeqParserTest | | 🟡 | Basic tests use parseAndCheckBoth, complex tests still V1-only |
| ObjectParserTest | | ✅ | All tests use parseAndCheckBoth |
| DotParserTest | | ✅ | All tests use parseAndCheckBoth |
| VarargParserTest | | ✅ | All tests use parseAndCheckBoth |
| SimpleFunctionCallTest | | ✅ | All tests use parseAndCheckBoth |
| TupleAndFunctionCallTest | | ✅ | All tests use parseAndCheckBoth |
| ParserTest | ✅ | | Uses parseAndCheck (V1 only) |
| SimpleOpSeqTest | | ✅ | Uses parseAndCheckBoth |
| TelescopeParserTest | ✅ | | Uses parseAndCheck (V1 only) |
| CommentParserTest | | ✅ | All tests use parseAndCheckBoth |
| SimplePatternMatchingTest | | ✅ | Uses parseAndCheckBoth |

### Test Function Usage
- `parseAndCheck` / `parseAndCheckV0`: Runs tests against V1 (original reader) only
- `parseAndCheckBoth`: Runs tests against both V1 and V2 parsers
- `parseAndCheckV1`: Deprecated alias for parseAndCheckBoth

### Currently Passing Tests in Both V1 & V2
```scala
// Pattern Matching Tests
simple pattern matching with case statements
parse pattern matching with multiple cases

// Operator Sequence Tests
parse simple opSeq with single operator
parse opSeq with multiple operators
parse opSeq with mixed operators
parse prefix and postfix operators
parse mixfix expressions

// Object Tests
parse empty object
parse object with single field
parse object with multiple fields
parse nested objects

// Function Call Tests
parse simple function call
parse function call with arguments
parse nested function calls

// Dot Notation Tests
parse simple dot call
parse dot call with arguments
parse nested dot calls

// Other Tests
parse varargs
parse comments
```

### Tests Still V1-Only (Need Migration)
- Complex operator sequence tests (prefix, mixfix)
- Telescope parsing tests
- Error handling tests
- Source position tracking tests

### Migration Strategy
1. Start with `parseAndCheck` (V1-only)
2. Once test passes in V2, upgrade to `parseAndCheckBoth`
3. Document any V2-specific changes or improvements
4. Remove V1-only tests once all features are supported in V2

## Implementation Plan

1. **Phase 1: Core Functionality** (Current)
   - [x] Basic literal parsing
   - [x] Simple function calls
   - [x] Basic operator sequence parsing (flat OpSeq nodes)
   - [x] Remove special case handling for keywords
   - [x] Implement uniform symbol treatment
   - [🟡] Migrate V1-only tests to V2 (In Progress)

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

3. **Pattern Matching Newline Rules**
   - Single expressions: `case X => expr;`
     - Semicolon required after expression
     - Newlines are treated like spaces (not separators)
     - Example:
       ```
       case A => expr1;
       case B => expr2;
       ```
   - Block expressions: `case X => { stmts }`
     - No semicolon needed after block
     - Newline after block acts as separator
     - Example:
       ```
       case A => {
         stmt1;
         stmt2
       }
       case B => expr2;
       ```
   - Comments and newlines in pattern matching:
     - Comments before case are preserved
     - Newlines between cases are preserved
     - Example:
       ```
       // First case
       case A => expr1;
       // Second case
       case B => expr2;
       ```

4. **Incremental Parsing**
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