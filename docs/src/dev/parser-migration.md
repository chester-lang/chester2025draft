# Chester Parser Architecture and Migration Status

## Overview

Chester is migrating from the original `reader` implementation to a new `readerv2`. This document covers both the migration status and the key design decisions in our parser architecture.

## Syntax Design Principles

### 1. Uniform Symbol Treatment
- All identifiers and operators are treated uniformly in parsing
- No special cases for keywords like "if", "then", "else" - they are just identifiers
- Parser doesn't distinguish between keywords and regular identifiers
- Semantic meaning determined in later passes
- Examples:
  ```scala
  // These parse exactly the same way:
  case x => y
  myCase x => y
  
  // Both produce:
  OpSeq([identifier, expr, identifier, expr])
  ```

### 2. Operator and Identifier Rules
- Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
- Identifiers start with letters/emoji/underscore
- Both can contain operator symbols and word symbols
- Key rules:
  - No special casing of operators - determined by character patterns
  - All operator identification delegated to consistent rules
  - Operators and identifiers form a uniform sequence: `expr op expr op expr ...`
  ```scala
  1 + 2          // OpSeq([1, +, 2])
  if x then y    // OpSeq([if, x, then, y])  // "if" and "then" are just identifiers
  val x = 1      // OpSeq([val, x, =, 1])    // "val" is just an identifier
  ```

### 3. Separation of Concerns
- Parser only produces flat OpSeq nodes without any knowledge of operator semantics
- Operator precedence, fixity (infix/prefix/postfix), and associativity are handled in later passes
- This separation allows flexible operator definition and extension
- No predefined keywords (if/then/else/val) or operators (+/-/*)
- Example parsing:
  ```scala
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

### 4. Space and Newline Handling

#### Space Significance
- Spaces are significant in specific contexts:
  - Function calls: `f()` vs `f ()` - space before parentheses changes interpretation
    - Without space: Parsed as function call
    - With space: Parsed as separate identifier and tuple
  - Operator sequences: Spaces don't affect operator precedence
    - `a + b` is equivalent to `a+b`
    - Spaces around operators are for readability only

#### Newline Significance
- Newlines have special significance in specific contexts:

  1. **After blocks**: Newline after a block ends the expression
     ```scala
     // Two separate expressions
     f { aaa }
     b { bbb }
     
     // Single expression
     f { aaa } b { bbb }
     ```

  2. **Pattern Matching**:
     - For single expressions: `case X => expr;`
       - Semicolon required after expression
       - Newlines are treated like spaces (not separators)
     - For blocks: `case X => { stmts }`
       - Only need newline after block (}\n)
       - Newline after block acts as separator
       - No semicolon needed after block
     ```scala
     x match {
       case1 => expr1;
       case2 => {
         stmt1;
         stmt2
       }
       case3 => expr3;
     }
     ```

  3. **Within blocks**: Newlines within blocks are not significant
     - Expressions can span multiple lines within a block
     - Operators and operands can be split across lines
     ```scala
     f {
       1 +
       2
     }
     ```

### 5. Block Return Value Semantics
- Rust-like block return value semantics
- Last expression in block is the return value
- Examples:
  - `{a}` -> returns value of a
  - `{a;}` -> equivalent to `{a; ()}` -> returns unit
  - `{a; b}` -> returns value of b
  - `{a; b;}` -> equivalent to `{a; b; ()}` -> returns unit

```scala
{ println(1); 2 }     // returns 2
{ println(1); 2; }    // returns unit
{ println(1) }        // returns println result
{ println(1); }       // returns unit
```

### 6. Error Handling
- Designed to handle incomplete/broken source code
- Produces meaningful partial results when possible
- Uses ErrorExpr to represent recoverable parse errors

### 7. Incremental Parsing
- Supports partial parsing of incomplete expressions
- Maintains parser state for potential incremental updates
- Useful for IDE integration

## Parser Implementations

### Original Parser Design (V1)
- Uses FastParse combinators for a declarative style
- Context object tracks parsing state
- Functional composition of parsers
- Fully functional and used in production

### ReaderV2 Approach (V2)
- Token-based parsing with state machine
- More imperative style with mutable state
- Pattern matching on tokens for different constructs
- Explicit recursion with depth tracking

### Implementation Benefits
Both implementations maintain the philosophy of uniform symbol treatment, with these advantages:
- Simpler, more maintainable parser
- Allows user-defined keywords and operators
- Consistent parsing rules with no special cases
- Flexible operator definition and extension
- Single source of truth for identifier rules
- Pattern matching as regular operator sequences

## Migration Status

### Feature Comparison

| Feature | Reader (V1) | ReaderV2 (V2) | Notes |
|---------|-------------|---------------|-------|
| Basic Literals | ✅ | ✅ | Integers, floating-point numbers fully supported |
| Function Calls | ✅ | ✅ | Full support in V2 |
| Pattern Matching | ✅ | ✅ | Now supports uniform treatment |
| Object Syntax | ✅ | 🟡 | Basic support in V2 |
| Operator Sequence | ✅ | ✅ | Parser produces flat OpSeq nodes |
| Error Recovery | ✅ | 🔴 | Planned for V2 |
| Source Maps | ✅ | 🔴 | To be implemented |
| Unicode Support | ✅ | ✅ | Full support in both |
| Generic Type Parameters | ✅ | ✅ | Full support including complex and nested generics |
| Block Arguments | ✅ | ✅ | Block calls now properly supported |
| Lists with Mixed Types | ✅ | ✅ | Now properly supported including floating-point |

Legend:
- ✅ Fully Implemented
- 🟡 Partially Implemented
- 🔴 Not Yet Implemented

### Test Coverage

| Test File | V1 Only | Both V1 & V2 | Notes |
|-----------|---------|--------------|-------|
| OpSeqParserTest | 🟡 | 🟡 | Most tests use parseAndCheckBoth, only "parse infix with block" uses V1-only due to semantic differences |
| ObjectParserTest | | ✅ | All tests use parseAndCheckBoth |
| DotParserTest | | ✅ | All tests use parseAndCheckBoth |
| VarargParserTest | | ✅ | All tests use parseAndCheckBoth |
| SimpleFunctionCallTest | | ✅ | All tests use parseAndCheckBoth |
| TupleAndFunctionCallTest | | ✅ | All tests use parseAndCheckBoth |
| ParserTest | | ✅ | All tests now use parseAndCheckBoth, including floating-point literals |
| SimpleOpSeqTest | | ✅ | Uses parseAndCheckBoth |
| TelescopeParserTest | | ✅ | All tests now use parseAndCheckBoth |
| CommentParserTest | | ✅ | All tests use parseAndCheckBoth |
| SimplePatternMatchingTest | 🟡 | 🟡 | Some tests still use parseAndCheck |
| ListParserTest | | ✅ | All tests now use parseAndCheckBoth, including mixed types with floating-point |
| BlockAndBlockCallParserTest | | ✅ | All tests use parseAndCheckBoth |
| FunctionCallParserTest | | ✅ | All tests now use parseAndCheckBoth, including complex generic type parameters |
| PatternMatchingTest | 🟡 | 🟡 | Some tests still use parseAndCheck |

### Test Function Usage
- `parseAndCheck` / `parseAndCheckV0`: Runs tests against V1 (original reader) only
- `parseAndCheckBoth`: Runs tests against both V1 and V2 parsers

### Currently Passing Tests in Both V1 & V2
- Pattern Matching: simple case statements, multiple cases
- Operator Sequences: simple and complex sequences, prefix/postfix/mixfix operators
- Objects: empty objects, single/multiple fields, nested objects, mixed types
- Function Calls: simple calls, with arguments, nested calls, mixed arg types
- Dot Notation: simple dot calls, with arguments, nested dot calls
- Tuples: with type annotations, function calls, identifier conversions
- Varargs: function calls and definitions with varargs
- Literals: integers (decimal, hex, binary), floating-point, strings
- Lists: empty lists, single items, mixed types, nested lists

### Tests Still V1-Only (Need Migration)
- Complex operator sequence tests (prefix, mixfix)
- Telescope parsing tests
- Error handling tests
- Source position tracking tests
- Some block call tests with complex contexts
- Function calls with generic type parameters

## Implementation Plan

### Phase 1: Core Functionality (✅ Mostly Complete)
- ✅ Basic literal parsing
- ✅ Simple function calls
- ✅ Basic operator sequence parsing
- ✅ Uniform symbol treatment
- ✅ Support for floating-point numbers
- ✅ Lists with mixed types
- 🟡 Migrate V1-only tests to V2 (In Progress)

### Phase 2: Advanced Features (🟡 Current)
- ✅ Full block call support
- ✅ Generic type parameters
- 🔴 Complex object syntax
- 🔴 Telescope parsing
- 🔴 Source maps

### Phase 3: Error Handling (🔴 Planned)
- 🔴 Error recovery
- 🔴 Improved error messages
- 🔴 Source position tracking
- 🔴 Debug information

## Next Steps

### Immediate Tasks
- [x] Complete block calls handling
- [x] Improve floating-point number parsing
- [x] Migrate more tests to use parseAndCheckBoth
- [x] Migrate SimplePatternMatchingTest and PatternMatchingTest (keeping them on parseAndCheck due to semantic differences)
- [x] Address semantic differences between V1 and V2 for infix with block expressions (documented in OpSeqParserTest)
- [x] Improve generic type parameter parsing (added tests for complex and nested generic types)

### Future Work
- [ ] Implement error recovery
- [ ] Add source maps support
- [ ] Complete advanced features

### Documentation
- [x] Document new parser architecture
- [ ] Create migration guide
- [x] Update test documentation 
