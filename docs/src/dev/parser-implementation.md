# Chester Parser Implementation Details

This document covers key implementation strategies in the Chester V2 parser.

## Tokenizer Optimizations

### UTF-16 Position Handling
- Simplified position tracking with proper UTF-16 coordinate mapping
- Support for surrogate pairs and supplementary characters
- Benefits: reduced memory footprint, better emoji and Unicode handling

### Token Creation and Handling
- Centralized token creation with lookup tables and specialized handlers
- Token extractors using common helper functions for consistent behavior
- Benefits: reduced code duplication, more maintainable parsing logic

### Unicode and Emoji Support
- Unicode-aware codepoint handling instead of raw character operations
- Proper surrogate pair handling and position calculation
- Benefits: correct handling of all Unicode characters including emoji

## Key Parser Components

### State Management
- Comprehensive `LexerState` tracking with context awareness
- Efficient token handling with pending token collection
- Preservation of comment and whitespace information
- Context flags for block termination detection

### Comment Handling
- Optimized comment collection with `skipComments()` and `pullComments()` methods
- Proper attachment to expressions for documentation preservation
- Consistent handling across different parsing contexts
- Metadata merging for comment preservation

### Expression Parsing and Operator Sequences
- Context-free parsing with uniform symbol treatment
- **⚠️ IMPORTANT**: Chester parser **intentionally** produces flat OpSeq nodes **without** imposing operator semantics or precedence
  - This is a **fundamental design principle**, not a limitation
  - This is **easy to misunderstand** - many developers incorrectly assume precedence handling
  - The semantic analyzer handles precedence later, not the parser
- Token-based parsing with proper expression termination detection

### Block Termination Pattern Handling

The `}\n` pattern (closing brace followed by newline) is used for terminating expressions:

```
def factorial(n) {
  if n <= 1 then 1
  else n * factorial(n - 1)
}  // <- newline here ends the function definition

val result = factorial(5); // Next statement
```

Implementation approach:
- Context tracking in LexerState with `newLineAfterBlockMeansEnds` flag
- Whitespace analysis to detect newlines after closing braces
- Pattern detection in `parseRest()` method
- Uniform treatment for all blocks without special-casing operators
- Preserves Chester's uniform symbol treatment principles

### Object Expression Parsing
- Support for identifier, string literal, and symbol literal keys
- Handling of both `=` and `=>` operators in object clauses
- Proper type conversion for different key types
- Field parsing with comma-separated clauses
- Comprehensive error reporting for malformed objects

### Function Call and Block Argument Handling
- Special handling for blocks used as function arguments
- Context-aware parsing of function calls with block arguments
- Proper metadata preservation for source positions
- Consistent handling of nested function calls

## String and Number Parsing
- Character-by-character parsing with comprehensive escape sequence support
- Support for various number formats (decimal, hex, binary, floating-point)
- Robust validation and error reporting
- Metadata preservation for source positions

## Current Optimizations
- Token extractors using common helper functions
- Efficient whitespace and comment handling
- Context-aware parsing for better performance
- Reduced memory usage with streamlined token representation

## Future Optimizations
- Token stream buffering and specialized token handling
- Error recovery enhancements
- Further memory usage optimization
- Performance improvements for large files
