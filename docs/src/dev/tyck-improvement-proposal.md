# Type Checking System Improvement Proposal

This document outlines specific proposed improvements to Chester's type checking system, focusing on dependent types and edge cases discovered during implementation.

## Background

Building on the improvements documented in `dependent-type-implementation-plan.md`, we've identified additional areas where the type checking system can be enhanced to better support dependent types and improve overall robustness.

## Current Status

Key improvements already made:
- Enhanced the `reduceTypeStructure` method in `NaiveReducer` to properly handle complex type structures
- Fixed the `areAlphaEquivalent` method in `TyckPropagator` to properly fall back to equality checking
- Improved handling of `TelescopeTerm` objects in function types for alpha-equivalence checks

## Proposed Improvements

### 1. Add Specialized Test Cases for Dependent Types

**Problem**: While the general type checking code is improved, we lack specific tests that verify dependent type behavior.

**Proposed Solution**: Create a dedicated test file `DependentTypeTest.scala` with test cases that verify:

- Function types with type dependencies
- Equality of types with different variable names but same structure
- Union and intersection types with alpha-equivalent components
- Nested dependent type expressions

**Implementation Steps**:
1. Create `semantic/shared/src/test/scala/chester/tyck/DependentTypeTest.scala`
2. Implement test cases for each scenario above
3. Run tests to verify behavior

### 2. Improve Variable Binding Handling in Alpha-Equivalence

**Problem**: The current alpha-equivalence implementation doesn't fully account for variable binding context when comparing terms. This can lead to issues with dependent types where variable names and contexts matter.

**Proposed Solution**: Enhance `areAlphaEquivalent` to maintain a mapping between bound variables:

```scala
private def areAlphaEquivalent(
    lhs: Term,
    rhs: Term,
    boundVars: Map[LocalV, LocalV] = Map()
)(using state: StateAbility[Tyck], localCtx: Context): Boolean = {
  // Implementation with mapping between bound variables
}
```

**Implementation Steps**:
1. Update `areAlphaEquivalent` signature to include bound variable mapping
2. Modify the logic to track variable bindings
3. Use this mapping when checking result types in function types
4. Add tests that verify correct handling of alpha-conversion

### 3. Enhance Record Type Field Access with Dependent Types

**Problem**: Type-level record field access with dependent types may not be properly handled.

**Proposed Solution**: Update the record field access logic to apply proper type reduction:

```scala
case class RecordFieldPropagator(
    recordTy: CellId[Term],
    fieldName: Name,
    expectedTy: CellId[Term],
    cause: Expr
)(using Context) extends Propagator[Tyck] {
  // Enhanced implementation with dependent type handling
}
```

**Implementation Steps**:
1. Update `RecordFieldPropagator` to better handle reduced field types
2. Ensure type-level computations in record fields are properly reduced
3. Add tests with dependent record fields

### 4. Document Alpha-Equivalence Concepts in Codebase

**Problem**: The implementation of alpha-equivalence checking is not well-documented, making it harder for contributors to understand and maintain.

**Proposed Solution**: Add comprehensive documentation to the relevant methods:

```scala
/**
 * Checks if two terms are alpha-equivalent.
 * 
 * Alpha-equivalence is a concept from lambda calculus that considers terms equivalent
 * if they are identical up to consistent renaming of bound variables. This is crucial
 * for dependent type systems where types can contain terms and binding structure matters.
 * 
 * Examples:
 * - (x: Type) -> x and (y: Type) -> y are alpha-equivalent
 * - (x: Type) -> (y: x) -> y and (a: Type) -> (b: a) -> b are alpha-equivalent
 * 
 * @param lhs First term to compare
 * @param rhs Second term to compare
 * @return true if terms are alpha-equivalent, false otherwise
 */
private def areAlphaEquivalent(lhs: Term, rhs: Term)(using ...): Boolean = {
  // Implementation
}
```

**Implementation Steps**:
1. Add comprehensive documentation to `areAlphaEquivalent`
2. Add documentation to `typesEquivalentModuloOrdering`
3. Add documentation to `tryUnify` specifically about alpha-equivalence checks

## Implementation Plan

### Phase 1: Testing and Documentation

1. Create `DependentTypeTest.scala` with basic test cases
2. Add comprehensive documentation to existing alpha-equivalence methods
3. Document the design rationale for handling dependent types

### Phase 2: Enhanced Alpha-Equivalence

1. Update `areAlphaEquivalent` to handle bound variable mapping
2. Add test cases for this enhanced functionality
3. Verify that existing tests still pass

### Phase 3: Record Field Access

1. Update `RecordFieldPropagator` to better handle dependent types
2. Add test cases for record fields with dependent types
3. Verify that all tests pass

## Risks and Mitigations

- **Risk**: Changes to alpha-equivalence could break existing code
  - **Mitigation**: Comprehensive test coverage including regression tests

- **Risk**: Performance impact from more complex equivalence checking
  - **Mitigation**: Optimize implementation, consider memoization for repeated checks

## Success Criteria

1. All tests pass, including new dependent type-specific tests
2. Documentation clearly explains dependent type concepts
3. The type checking system correctly handles complex dependent type scenarios 