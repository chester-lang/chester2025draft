# Chester Parser Architecture and Improvement Plan

## Overview

Chester is migrating from the original `reader` implementation (V1) to a new `readerv2` (V2). This document covers both the migration status and the planned improvements to enhance readability, maintainability, and performance of the parser components.

## Parser Development Guidelines

### Core Principles

1. **Maintain Context-Free Parsing**
   - ALWAYS prefer context-free over context-dependent parsing approaches
   - Avoid parser decisions that depend on the semantic meaning of tokens
   - Use simple, uniform rules for terminating expressions (e.g., `}\n` pattern)
   - Treat all identifiers uniformly regardless of their potential semantic meaning
   - The parser should not try to "understand" what specific identifiers like 'match', 'if', or 'val' mean

2. **Separation of Concerns**
   - Maintain strict separation between parsing (syntax) and semantic analysis
   - The parser only concerns itself with syntactic structure, not meaning
   - Operator precedence, fixity, and associativity are handled in later passes, NOT in the parser
   - Parser should produce flat OpSeq nodes without interpreting operator semantics
   - Semantic meaning is determined by subsequent compilation phases

3. **Verification Practices**
   - Follow the git verification practices from development.md
   - For parser changes specifically:
     - Verify that changes maintain context-free parsing principles
     - Check that uniform symbol treatment is preserved
     - Ensure no special handling for specific identifiers was introduced
     - Test both parsers (V1 and V2) using parseAndCheckBoth where possible

### Common Parser Development Pitfalls

1. **Adding Context-Dependent Parsing**
   - DON'T make parsing decisions based on identifier meanings
   - DON'T add special cases for specific keywords or identifiers
   - DON'T vary parsing behavior based on semantic context

2. **Premature Operator Resolution**
   - DON'T attempt to handle operator precedence within the parser
   - DON'T transform flat OpSeq nodes into nested structures during parsing
   - Keep operator resolution as a separate pass after parsing

3. **Inconsistent Newline Handling**
   - DON'T use different newline handling rules in different contexts
   - DO apply the same syntactic rules for expression termination everywhere
   - Ensure consistent behavior for `}\n` patterns regardless of surrounding code

4. **Identifier Special-Casing**
   - DON'T treat 'match', 'if', 'val', etc. as special during parsing
   - DO treat all identifiers uniformly as plain identifiers
   - Remember: parsing is about syntax structure, not identifier semantics

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
       }  // <- newline here ends the function definition
       
       val result = factorial(5); // Next statement
       ```
    2. Match expressions: Newline after closing brace marks end of the match expression
       ```
       val result = notification match {
         case Email(sender, _) => {
           println(sender);
           "Email received"
         }  // <- block for this case
         case SMS(number, _) => "SMS received";
       }  // <- newline here ends the match expression
       
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

## Comment Preservation Implementation

### Current State Analysis

The V2 parser has comment token representation and support for attaching comments to expressions in the AST. Here are the key components:

1. **Token Representation**: 
   - Comments are represented as `Token.Comment(text: String, sourcePos: SourcePos)` tokens during lexing
   - The parser handles comments during parsing and preserves them in the AST

2. **Comment Structure in AST**:
   - `chester.syntax.concrete.Comment` class with content, type (OneLine/MultiLine), and sourcePos
   - `chester.syntax.concrete.CommentInfo` class with collections for:
     - commentBefore: Comments before an expression
     - commentInBegin: Comments at the beginning of a block structure
     - commentInEnd: Comments at the end of a block structure
     - commentEndInThisLine: Comments at the end of a line with an expression

3. **Metadata Handling**:
   - `ExprMeta` contains sourcePos and optional commentInfo
   - `MetaFactory` has helper methods for comment attachment
   - Expression nodes have methods like `commentAtStart`, `updateMeta` for comment handling

### Implementation Components

1. **Comment Collection**
   - Methods to collect and categorize comments during parsing
   - Conversion of Token.Comment instances to chester.syntax.concrete.Comment objects
   - Preservation of source positions and comment text
   - Categorization based on surrounding whitespace/newlines

2. **Metadata Creation with Comments**
   - Methods to create expression metadata with leading and trailing comments
   - Integration with ExprMeta and CommentInfo structures

3. **Comment Association with Expressions**
   - Modified expression creation methods to include comment attachment
   - Handling of both leading and trailing comments

4. **Special Cases Handling**
   - Block comments: Comments within blocks attached to the block or nearest expression
   - End-of-line comments: Comments following an expression on the same line
   - Standalone comments: Comments not directly associated with an expression
   - Comments between expressions: Determination of which expression they belong to

5. **Testing Strategy**
   - Test cases with various comment placements:
     - Leading comments before expressions
     - Trailing comments after expressions
     - Comments within blocks, lists, and objects
     - Standalone comments between expressions
   - Verification of comment content and positions preservation
   - Comparison with V1 parser to ensure compatibility
   - Edge cases testing:
     - Multiple consecutive comments
     - Comments with special characters
     - Comments at file beginning and end

### Compatibility Considerations

- Backward compatibility with V1 parser's comment representation
- AST consumers that use comment information
- Code formatting tools that rely on comment positions

All tests in CommentParserTest now use parseAndCheckBoth to verify that both parsers correctly preserve comments.

## Implementation Plan

### Phase 1: Core Functionality (✅ Complete)
- ✅ Basic literal parsing
- ✅ Simple function calls
- ✅ Basic operator sequence parsing
- ✅ Uniform symbol treatment
- ✅ Support for floating-point numbers
- ✅ Lists with mixed types
- ✅ Migrate V1-only tests to V2 for basic functionality

### Phase 2: Advanced Features (🟡 Current)
- ✅ Full block call support
- ✅ Generic type parameters
- ✅ Comment preservation and attachment
- ✅ Uniform operator handling without special cases
- 🟡 Object expressions with string literal and symbol keys
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

## Recent Improvements

### 7. Uniform Operator Handling ✅ COMPLETED
- **Issue**: Special case handling for the "=>" and "=" operators in `parseOperator()` method
- **Improvement**: 
  - Removed special case handling for the "=>" operator
  - Ensured operators are treated uniformly in the tokenizer
  - Treated "=>" like any other operator in the tokenizing process
- **Benefits**: 
  - More consistent operator handling
  - Simplified code in the `parseOperator()` method
  - Reduced special cases, making the code more maintainable
  - Better alignment with Chester's design principles of uniform symbol treatment
- **Implementation**: 
  - Removed special case code for the "=>" operator in the `parseOperator()` method
  - Modified the method to uniformly parse all operators using a `StringBuilder`
  - Verified all tests pass with the change, including operator tests
  - Ensured consistent behavior with the original implementation

### 8. LexerV2 Optimization and Refactoring ✅ COMPLETED
- **Issue**: `LexerV2.scala` had redundant code and a missing state.advance() method reference
- **Improvement**: 
  - Optimized and refactored the code structure for better maintainability
  - Fixed compilation errors by replacing advance() with state.advance()
  - Improved modularity by extracting repeated logic into helper methods
  - Enhanced state management for better consistency across the codebase
- **Benefits**: 
  - Improved maintainability and readability of the lexer code
  - Fixed compilation errors resulting in more stable code
  - Better organization of related functionality
  - Reduced duplication for easier future updates
- **Implementation**: 
  - Replaced direct advance() calls with state.advance() where appropriate
  - Restructured code for better organization and clarity
  - Maintained functionality while improving code quality
  - Ensured all tests continued to pass after changes

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

### 2. Object Expressions ⚠️ PRIORITY
- **Issue**: Object expressions support in V2 is incomplete, especially for complex objects
- **Improvement**: 
  - Complete object expressions implementation with string literal and symbol keys
  - Ensure complex object syntax works properly
  - Handle advanced object features

### 3. Block Termination and Newline Handling ⚠️ PRIORITY
- **Issue**: Inconsistent handling of the `}\n` pattern between V1 and V2 parsers
- **Solution**: 
  - Implement whitespace enhancement with newline flag
  - Update expression termination logic to handle `}\n` pattern consistently

## Next Steps

1. Address V1/V2 Semantic Consistency to ensure V2 parser correctly implements V1 semantics
   - Focus on complex operator sequences (prefix, mixfix) first
   - Address pattern matching semantic differences
   - Implement proper telescope parsing
2. Implement the whitespace enhancement with newline flag for better `}\n` pattern handling
3. Complete object expressions implementation with complex syntax support
4. Add source maps support
5. Continue migration of all remaining V1-only tests to V2
6. Implement error recovery mechanisms 

## Historical Notes
<!-- These historical implementation details have been moved to devlog.md for reference -->

### Previously Completed Improvements

1. **Comment Preservation** ✅
   - Added comment collection and attachment similar to V1 parser
   - Implemented leading/trailing comment support
   - Verified compatibility with V1 parser behavior

2. **Number Parsing Refactoring** ✅
   - Extracted specialized methods for different number formats
   - Improved error handling for number parsing

3. **Enhanced Escape Character Handling** ✅
   - Extended support for escape sequences (Unicode, octal, hex)
   - Improved error reporting for invalid escape sequences

4. **Basic Operator Parsing Clean-Up** ✅
   - Extracted comment parsing to a separate method
   - Improved structure of `parseOperator()` method

5. **Identifier Parsing Correctness** ✅
   - Aligned with IdentifierRules for consistent character validation
   - Improved handling of Unicode characters and emoji

6. **SourcePos Creation Efficiency** ✅
   - Implemented caching for UTF-16 offset calculations
   - Reduced tokenization time for complex expressions

### Implementation Details Archive

#### The `}\n` Pattern Problem
The Chester parser treats the `}\n` pattern as significant for terminating expressions in specific contexts.

##### Proposed Approaches:
1. **Context-Free Newline Handling (PREFERRED)**
   - Always end OpSeq expression when encountering `}\n`
   - Apply this rule uniformly regardless of context

2. **Token Differentiation Strategy**
   - Enhance tokenizer to differentiate between whitespace and newlines

3. **Whitespace with Newline Flag**
   - Add a `canActAsNewline` flag to whitespace tokens

##### Detailed Implementation Strategy:
1. Enhance whitespace token handling
2. Modify expression termination logic for `}\n` pattern
3. Ensure uniform treatment across contexts
4. Add specific test cases for verification

## Known Issues and Solutions

### The `}\n` Pattern Issue
The inconsistent handling of the `}\n` pattern between V1 and V2 parsers remains a key issue to solve. This affects:

1. **Function/Method Definitions**: Where newlines after closing braces mark the end of definitions
2. **Match Expressions**: Where newlines after closing braces terminate match expressions

The solution requires enhancing whitespace token handling and implementing consistent expression termination rules.

### Pattern Matching Block Termination Fix

#### Current Status
Tests in `PatternMatchingTest.scala` have revealed inconsistencies between V1 and V2 parsers specifically around pattern matching with blocks:
- Simple pattern matching cases work correctly with both parsers
- Pattern matching with blocks after `=>` operators only passes in V1

#### Root Cause
The issue relates to how the `}\n` pattern is handled in the context of pattern matching:

1. **V1 Implementation**: Uses `ParsingContext(newLineAfterBlockMeansEnds = true)` to define how block termination works
2. **V2 Implementation**: Has a more general `checkForRBraceNewlinePattern` mechanism that doesn't properly account for pattern matching contexts

#### Implementation Plan

**IMPORTANT: General Handling Approach Required**
Our implementation MUST follow Chester's principle of uniform symbol treatment. We SHOULD NOT add special handling for the `=>` operator specifically. Instead, we should implement a general solution for consistent `}\n` pattern handling that works across ALL contexts, including but not limited to pattern matching.

The `}\n` pattern should be treated uniformly based on syntactic structure, NOT on the semantic meaning of operators like `=>`. This ensures the parser remains truly general without special cases.

1. **Enhanced Context Tracking**:
   - Add context tracking to `LexerState` to know when newlines after blocks should terminate expressions
   - Maintain immutable state through appropriate helper methods
   - Keep uniform treatment without special-casing operators

2. **Context-Aware Block Termination**:
   - Update the block termination logic to consider the context flag
   - Detect `}\n` patterns based on pure syntax, not semantics

3. **Expression Structure Preservation**:
   - Ensure consistent AST structure between V1 and V2 parsers
   - Preserve the AST structure for pattern matching expressions
   - Match V1 behavior without operator-specific logic

This approach will ensure compatibility between V1 and V2 parsers while maintaining strict adherence to the principle of general, uniform handling of syntax patterns.

## Future Optimization Opportunities