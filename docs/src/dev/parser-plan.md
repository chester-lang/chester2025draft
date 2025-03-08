# Chester Parser Architecture and Improvement Plan

## Overview

Chester is migrating from the original `reader` implementation (V1) to a new `readerv2` (V2). This document covers both the migration status and the planned improvements to enhance readability, maintainability, and performance of the parser components.

## Parser Architecture

### Syntax Design Principles

#### 1. Uniform Symbol Treatment
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

#### 2. Operator and Identifier Rules
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

#### 3. Separation of Concerns
- Parser only produces flat OpSeq nodes without any knowledge of operator semantics
- Operator precedence, fixity (infix/prefix/postfix), and associativity are handled in later passes
- This separation allows flexible operator definition and extension
- No predefined keywords (if/then/else/val) or operators (+/-/*)

#### 4. Space and Newline Handling
- Spaces are significant in specific contexts:
  - Function calls: `f()` vs `f ()` - space before parentheses changes interpretation
  - Operator sequences: Spaces don't affect operator precedence but improve readability
- Newlines have special significance in specific contexts:
  - After blocks: Newline after a block ends the expression (i.e., `}\n` terminates the expression)
  - Control structures: Newlines are treated differently based on context, not on specific keywords
  - Pattern matching, conditionals, etc.: The behavior is consistent across all control structures 
  - Within blocks: Newlines within blocks are not significant
  - Syntax patterns like `}\n` (closing brace followed by newline) have context-dependent meanings
  - Two key cases where `}\n` has special semantic significance:
    1. Function/method definitions: Newline after closing brace marks end of the definition
       ```
       def factorial(n) {
         if n <= 1 then 1
         else n * factorial(n - 1)
       } // <- newline here ends the function definition
       
       val result = factorial(5); // Next statement
       ```
    2. Match expressions: Newline after closing brace marks end of the match expression
       ```
       val result = notification match {
         case Email(sender, _) => {
           println(sender);
           "Email received"
         } // <- block for this case
         case SMS(number, _) => "SMS received";
       } // <- newline here ends the match expression
       
       println(result); // Next statement
       ```

#### 5. Block Return Value Semantics
- Rust-like block return value semantics
- Last expression in block is the return value
- Examples:
  - `{a}` -> returns value of a
  - `{a;}` -> equivalent to `{a; ()}` -> returns unit
  - `{a; b}` -> returns value of b
  - `{a; b;}` -> equivalent to `{a; b; ()}` -> returns unit

#### 6. Error Handling
- Designed to handle incomplete/broken source code
- Produces meaningful partial results when possible
- Uses ErrorExpr to represent recoverable parse errors

#### 7. Incremental Parsing
- Supports partial parsing of incomplete expressions
- Maintains parser state for potential incremental updates
- Useful for IDE integration

### Parser Implementations

#### Original Parser Design (V1)
- Uses FastParse combinators for a declarative style
- Context object tracks parsing state
- Functional composition of parsers
- Fully functional and used in production

#### ReaderV2 Approach (V2)
- Token-based parsing with state machine
- More imperative style with mutable state
- Pattern matching on tokens for different constructs
- Explicit recursion with depth tracking

## Migration Status

### Feature Comparison

| Feature | Reader (V1) | ReaderV2 (V2) | Notes |
|---------|-------------|---------------|-------|
| Basic Literals | ✅ | ✅ | Integers, floating-point numbers fully supported |
| Function Calls | ✅ | ✅ | Full support in V2 |
| Pattern Matching | ✅ | ✅ | Now supports uniform treatment |
| Object Syntax | ✅ | 🟡 | Basic support in V2 |
| Operator Sequence | ✅ | ✅ | Parser produces flat OpSeq nodes |
| Error Recovery | ✅ | 🟡 | Comment preservation implemented, other error recovery planned |
| Source Maps | ✅ | 🔴 | To be implemented |
| Unicode Support | ✅ | ✅ | Full support in both |
| Generic Type Parameters | ✅ | ✅ | Full support including complex and nested generics |
| Block Arguments | ✅ | ✅ | Block calls now properly supported |
| Lists with Mixed Types | ✅ | ✅ | Now properly supported including floating-point |
| Comment Preservation | ✅ | ✅ | Now fully supported in V2 with leading and trailing comments |

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
| CommentParserTest | | ✅ | All tests use parseAndCheckBoth and verify comment preservation |
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
- ✅ Comment preservation and attachment
- 🟡 Object expressions with string literals and symbol keys
- 🔴 Complex object syntax
- 🔴 Telescope parsing
- 🔴 Source maps

#### Key Implementation Principles for V2
- Maintain uniform symbol treatment across all expressions
- Handle operator sequences consistently, avoiding special cases
- Support string literals and symbols as object field keys
- Avoid special case logic in operator sequence parsing
- Ensure field operators (= and =>) are treated consistently across key types
- Preserve the same semantics as V1 parser but with cleaner implementation

### Phase 3: Error Handling (🔴 Planned)
- 🔴 Error recovery
- 🔴 Improved error messages
- 🔴 Source position tracking
- 🔴 Debug information

## Completed Improvements

### 1. Comment Preservation ✅ COMPLETED
- **Issue**: The V2 parser previously didn't properly preserve comments and attach them to expressions via ExprMeta.commentInfo
- **Improvement**: Implemented comment collection and attachment similar to the V1 parser (Parser.scala)
- **Benefits**: 
  - Preserves important documentation in the code
  - Enables proper code formatting with comments
  - Maintains semantic information that may be in comments
- **Implementation Details**:
  1. Added comment collection methods (`collectComments`, `collectTrailingComments`)
  2. Created appropriate CommentInfo structures for leading and trailing comments
  3. Attached comments to expressions using ExprMeta with `createMetaWithComments`
  4. Implemented comment-aware parsing methods (`parseAtomWithComments`, `parseBlockWithComments`, `parseListWithComments`)
  5. Fixed whitespace token handling for proper newline detection
  6. All 100 tests now pass, confirming compatibility with V1 parser behavior

### 2. Number Parsing Refactoring ✅ COMPLETED
- **Issue**: The number parsing logic in `parseNumber()` was complex and had nested conditionals
- **Improvement**: Extracted specialized methods for different number formats:
  - `parseDecimalNumber()`
  - `parseHexNumber()`
  - `parseBinaryNumber()`
  - `parseExponent()`
- **Benefits**: Improved readability, easier maintenance, better error handling
- **Implementation**: Extracted four methods from the original complex code, improving error messages and making the logic more modular.

### 3. Enhanced Escape Character Handling ✅ COMPLETED
- **Issue**: The `escapeCharToString()` method only handled basic escape sequences
- **Improvement**: Extended to support more escape sequences including:
  - Unicode escapes (`\u1234`)
  - Octal escapes (`\123`)
  - Hex escapes (`\x12`)
- **Benefits**: More comprehensive string support, better consistency
- **Implementation**: Created a new `parseEscapeSequence()` method with comprehensive escape handling, better error reporting, and proper position tracking.

### 4. Operator Parsing Clean-Up ✅ COMPLETED
- **Issue**: Special-case handling for `=>` and comments within `parseOperator()`
- **Improvement**: Extracted comment parsing to a separate method
- **Benefits**: Cleaner code structure, better separation of concerns
- **Implementation**: Extracted `parseComment()` method and improved `parseOperator()` with clearer structure and better comments.

### 5. Identifier Parsing Correctness ✅ COMPLETED
- **Issue**: Inconsistent character detection in `parseIdentifier()`
- **Improvement**: 
  - Aligned with `IdentifierRules.isIdentifierPart()` for character validation
  - Ensured proper handling of supplementary code points (Unicode/emoji)
  - Maintained consistency between entry points and internal validation
- **Benefits**: 
  - Correct parsing of all valid identifiers according to language specification
  - Consistent handling of Unicode characters and emoji
  - Better maintainability through unified validation rules
- **Implementation**: 
  - Added import for `isIdentifierPart` from `IdentifierRules`
  - Created helper method for proper character validation
  - Updated character checking logic to handle both ASCII and Unicode properly
  - Ensured supplementary code points were handled consistently

### 6. SourcePos Creation Efficiency ✅ COMPLETED
- **Issue**: The `createSourcePos()` method recalculated UTF-16 offsets on every call
- **Improvement**: 
  - Implemented a caching mechanism for UTF-16 offset calculations
  - Calculated offsets incrementally rather than from scratch each time
  - Added a position cache to store already computed offsets
  - Updated the cache as positions change during tokenization
- **Benefits**: 
  - Significant performance improvement, especially for large files
  - Reduced tokenization time for complex expressions
  - More responsive parsing for interactive environments
- **Implementation**: 
  - Added a position cache using HashMap to store UTF-16 offsets
  - Modified `createSourcePos()` to check the cache before calculating
  - Updated methods that modify position to use and update the cache
  - Added incremental calculation for new positions based on cached values

## Current Priorities

### 1. V1/V2 Semantic Consistency ⚠️ PRIORITY
- **Issue**: 
  - Some tests still use `parseAndCheck` (V1-only) instead of `parseAndCheckBoth`
  - This indicates potential semantic differences between V1 and V2 parsers
  - The goal is to ensure both parsers produce the same AST for the same input
- **Areas to Address**:
  - Complex operator sequences (prefix, mixfix operators)
  - Pattern matching with mixed expressions
  - Telescope parsing
  - Error handling consistency
  - Source position tracking
- **Implementation Plan**:
  1. Analyze test files still using `parseAndCheck` to identify semantic differences
  2. Prioritize addressing the complex operator sequence handling first
  3. Implement proper handling for prefix and mixfix operators in V2
  4. Test and verify with existing test cases
  5. Update tests to use `parseAndCheckBoth` once they pass
  6. Document any intentional semantic differences that won't be addressed
- **Benefits**:
  - More consistent parsing behavior between V1 and V2
  - Higher confidence in V2 parser for all use cases
  - Easier migration path from V1 to V2
  - More tests running against both parsers

### 2. Object Expressions ⚠️ PRIORITY
- **Issue**: Object expressions support in V2 is incomplete, especially for complex objects
- **Improvement**: 
  - Complete object expressions implementation with string literal and symbol keys
  - Ensure complex object syntax works properly
  - Handle advanced object features
- **Implementation Plan**:
  1. Review current object parsing implementation
  2. Identify missing features compared to V1
  3. Implement support for complex object syntax
  4. Test with a variety of object expressions

### 3. Telescope Parsing ⚠️ PRIORITY
- **Issue**: Telescope parsing is not yet implemented in V2
- **Improvement**: Implement telescope parsing in V2 to match V1 semantics
- **Implementation Plan**:
  1. Analyze V1 telescope parsing implementation
  2. Design and implement equivalent functionality in V2
  3. Test with existing telescope tests

### 4. Block Termination and Newline Significance ⚠️ PRIORITY
- **Issue**: 
  - V2 parser doesn't properly handle the `}\n` pattern in the same way as V1 parser
  - This affects pattern matching blocks, function definitions, and other block expressions
  - The key pattern `}\n` (closing brace followed by newline) should terminate expressions in specific contexts
  - Tests like `PatternMatchingTest.match2` fail due to AST structural differences
- **Current Implementation Differences**:
  - V1 Parser (in `Parser.scala`) uses `newLineAfterBlockMeansEnds` flag in `ParsingContext`
  - V1 detects block endings with `val itWasBlockEnding = p.input(index - 1) == '}'`
  - V1 checks for newlines after blocks and changes parsing behavior accordingly
  - V2 Parser (in `LexerV2.scala`) does not have equivalent context tracking
  - V2 treats whitespace tokens uniformly without distinguishing significant newlines after blocks
- **Improvement**: 
  - Implement context-aware newline handling in V2 parser
  - Make V2 produce the same AST structure as V1 for blocks followed by newlines
  - Ensure both parsers handle the pattern consistently, especially in match expressions
- **Implementation Plan**:
  1. Add a context parameter to V2 similar to V1's `newLineAfterBlockMeansEnds`
  2. Modify `parseBlock` and related methods to be aware of this context
  3. Update token handling to distinguish between regular whitespace and newlines after blocks
  4. Implement special handling for statement contexts (match expressions, etc.)
  5. Add specific tests for the `}\n` pattern in various contexts
  6. Ensure pattern matching blocks are handled correctly
- **Benefits**:
  - Consistent AST structure between V1 and V2 parsers
  - All pattern matching tests will pass with `parseAndCheckBoth`
  - Improved language clarity regarding block termination rules
  - Better documentation of newline significance in Chester syntax

## Implementation Strategy

1. Start with smaller, isolated improvements that don't affect the overall architecture ✅
2. Add comprehensive tests before making significant changes ✅
3. Update one component fully before moving to the next ✅
4. Prioritize improvements that enhance maintainability first ✅
5. Verify each change with existing tests before proceeding to the next improvement ✅
6. Complete high-priority features like comment preservation ✅
7. Update documentation to reflect implementation progress ✅

## Next Steps

1. Address V1/V2 Semantic Consistency to ensure V2 parser correctly implements V1 semantics
   - Focus on complex operator sequences (prefix, mixfix) first
   - Address pattern matching semantic differences
   - Implement proper telescope parsing
2. Complete object expressions implementation
3. Add source maps support
4. Continue migration of V1-only tests to V2
5. Implement error recovery mechanisms 