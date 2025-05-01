# Union Types Implementation

This document outlines the implementation and current state of union types in Chester.

## Current State

Union types are now fully implemented in Chester with both parsing and type checking support. The implementation can handle all three key subtyping relationships: union-to-union, specific-to-union, and union-to-specific.

## Implementation Overview

The following components have been implemented to support union types:

### 1. Parser & Desalting (`Desalt.scala`)

- **Union Type Parsing**: Implemented in the `desugar` method to handle union type expressions:
  - Detects `|` operators in type expressions
  - Creates a `UnionTypeExpr` node with component types
  - Handles nested union types properly
  - Preserves source position information for error reporting
  - Respects operator precedence and grouping

The parser now correctly recognizes syntax like:
```chester
// Simple union type
function accept(value: Integer | String) { ... }

// Nested union types
function process(data: Integer | String | (Array | Object)) { ... }

// Union in type annotation
let value: Integer | String = "hello";
```

### 2. Type Elaboration (`Elaborater.scala`)

- **Union Type Elaboration**: Implemented handling for union type expressions in the type checking system:
  - Elaborate each component type in the union
  - Create proper `Union` term with component types
  - Register appropriate propagators for type constraints
  - Maintain connections between union types and components

- **Union Type Unification**: Implemented support for all three subtyping relationships:
  - **Union-to-Union**: `(A|B) <: (C|D)` with proper component compatibility checks
  - **Specific-to-Union**: `A <: (B|C)` for cases like passing `Integer` to a parameter of type `Integer|String`
  - **Union-to-Specific**: `(A|B) <: C` for returning a union from a function with specific return type

- **Type-Level Functions with Union Types**: Added support for type-level functions that:
  - Return union types
  - Accept union types as arguments
  - Process union components correctly

The implementation enables code patterns like:

```chester
// Accepting a union type parameter
def process(value: Integer | String): String = {
  match value {
    case i: Integer => i.toString()
    case s: String => s
  }
}

// Returning a union type
def fetch(): Integer | String | Null = {
  if (hasData()) getData()
  else null
}

// Union types in generic contexts
def firstOrDefault[T](list: List[T], default: T): T = {
  if (isEmpty(list)) default else first(list)
}
```

### 3. Type Propagator (`TyckPropagator.scala`)

- **UnionOf Propagator Implementation**: Implemented the `UnionOf` propagator to handle union type constraints:
  - Manages relationships between union types and their components
  - Enforces proper subtyping relationships with union types
  - Handles cell coverage for all union components
  - Ensures proper zonking of union types

- **Enhanced Type Compatibility for Union Types**: Implemented union type compatibility with three key cases:
  
  1. **Union-to-Union Compatibility**:
     ```scala
     case (Union(types1, _), Union(types2, _)) => {
       // For union-to-union subtyping, we check component compatibility
       // with complex rules for determining when unions are subtypes of each other
     }
     ```
  
  2. **Term-to-Union Compatibility**:
     ```scala
     case (term, Union(types, _)) => {
       // For term-to-union subtyping, we check if the term is compatible
       // with any type in the union
     }
     ```
  
  3. **Union-to-Term Compatibility**:
     ```scala
     case (Union(types, _), term) => {
       // For union-to-term subtyping, we check if all types in the union
       // are compatible with the term
     }
     ```

- **Cell Coverage Implementation**: Added comprehensive cell coverage mechanisms:
  - Direct connections between union types and their components
  - Self-coverage for component types
  - Enhanced zonking capabilities for union types
  - Prevention of early returns that could leave cells uncovered

### 4. Test Framework and Validation

- **Comprehensive Test Suite**: Added a complete set of tests to validate union type functionality:
  - Basic union type syntax tests
  - Union type subtyping tests (all three relationship types)
  - Union type pattern matching tests
  - Function with union type parameters and return values
  - Cell coverage tests for union types
  - Edge cases and error handling tests

- **Test Files**:
  - `tests/tyck/simplest_union.chester`: Basic union type functionality
  - `tests/tyck/union-subtype.chester`: Union subtyping relationships
  - `tests/tyck/union-pattern-matching.chester`: Pattern matching with union types
  - Various integration tests using union types with other language features

## Current Status and Future Work

### Completed

1. ✅ Parser support for union type syntax
2. ✅ Type elaboration for union types
3. ✅ Full union type subtyping relationships
4. ✅ Cell coverage mechanisms for union types
5. ✅ Error reporting for union type mismatch errors
6. ✅ Integration with trait types and interfaces
7. ✅ Pattern matching with union types

### Future Enhancements

1. More comprehensive error messages for specific union type errors
2. Performance optimizations for complex union types
3. Enhanced type inference with union types
4. Integration with effect system
5. Compiler backend optimizations for union types

## References

- Main implementation files:
  - `Desalt.scala` (parser implementation)
  - `Elaborater.scala` (type checking implementation)
  - `TyckPropagator.scala` (propagator implementation)
  - `Term.scala` (union type representation)
  
- Test files:
  - `tests/tyck/simplest_union.chester`
  - `tests/tyck/union-subtype.chester`
  - Other integration test files

- Related documentation:
  - Type checking system documentation
  - Trait implementation documentation