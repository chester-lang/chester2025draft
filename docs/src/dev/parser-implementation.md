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

### Comment Handling
- Recursive comment collection approach for both leading and trailing comments
- Proper attachment to expressions for documentation preservation
- Consistent handling across different parsing contexts

### Expression Parsing and Operator Sequences
- Context-free parsing with uniform symbol treatment
- Flat OpSeq representation without imposing operator semantics
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
- Context tracking in LexerState for block termination awareness
- Pattern detection checking both context and token sequence
- Uniform treatment for all blocks without special-casing operators
- Preserves Chester's uniform symbol treatment principles

### Object Expression Parsing
- Support for identifier, string literal, and symbol literal keys
- Handling of both `=` and `=>` operators in object clauses
- Proper type conversion for different key types

## String and Number Parsing
- Character-by-character parsing with comprehensive escape sequence support
- Support for various number formats (decimal, hex, binary, floating-point)
- Robust validation and error reporting

## Future Optimizations
- Token stream buffering and specialized token handling
- Error recovery enhancements
- Memory usage optimization
