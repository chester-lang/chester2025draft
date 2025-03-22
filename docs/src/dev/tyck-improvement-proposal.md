# Comprehensive Type System Improvement Proposal

This document outlines specific proposed improvements to Chester's type checking system, focusing on dependent types, union and intersection types, and the integration between the reducer and type checker.

## 1. Overview and Background

Chester's type system is based on a constraint propagation network where:
- Type constraints are represented by **propagators**
- **Cells** hold type information and track their propagators
- Two types of propagator connections:
  - **Reading**: Propagators that read from a cell
  - **Zonking**: Propagators that can write to or resolve a cell

Recent improvements have focused on enhancing support for dependent types, which require:
1. Types that can depend on terms
2. Variable bindings in types with alpha-equivalence checking
3. Sophisticated reduction strategies for type equality

## 2. Current Status and Progress

**Note**: The key improvements implemented so far have been moved to the devlog entry for 2025-03-15. Refer to `docs/src/dev/devlog.md` for details on completed improvements.

## 3. Remaining Issues and Implementation Plan

### 3.1 Union Type Subtyping Implementation

While the basic implementation of union type subtyping has been completed, there are still some edge cases to resolve:

1. **Cell Coverage Issues**: When handling union types, cells occasionally lack proper propagator coverage, resulting in the error:
   ```
   java.lang.IllegalStateException: Cells Vector(...) are not covered by any propagator
   ```

2. **Union-Subtype Test Case**: The test file `union-subtype.chester.todo` still needs to pass successfully with the current implementation.

**Implementation Plan**:
1. Fix remaining edge cases in `union-subtype.chester.todo` (in progress)
2. Test all three union subtyping scenarios:
   - Union-to-Union subtyping
   - Specific-to-Union subtyping
   - Union-to-Specific subtyping
3. Rename test file to remove `.todo` suffix when passing

**Success Criteria**:
- All tests including union subtyping pass
- File `union-subtype.chester` type checks correctly

### 3.2 Enhanced Type-Level Function Application Reduction

#### Current Limitation

The current type checker supports basic type-level function applications, but has limited handling of nested or recursive function applications in type-level contexts. When complex type-level expressions involve multiple nested function calls, the reducer may not properly evaluate them during type checking, leading to:

1. Type errors due to incomplete reduction of nested function applications
2. Reduced flexibility when using type-level functions in complex ways
3. Unclear error messages when type-level function applications fail

#### Implementation Plan

The implementation requires focused changes to:

1. **NaiveReducer Enhancement**:
   - Improve handling of nested type-level function applications
   - Implement recursive handling of type-level function results
   - Ensure consistent reduction behavior for composed functions

2. **Type Checking Integration**:
   - Enhance the `tryUnify` method to handle function call terms
   - Ensure proper cell coverage for function applications
   - Add guards to prevent pattern matching conflicts

#### Testable Example

```chester
// Test enhanced type-level function application
record A(a: Integer);
record B(b: String);

// Basic identity function for types
def idType(x: Type): Type = x;

// Function composition at the type level
def composeTypes(f: Type -> Type, g: Type -> Type, x: Type): Type = f(g(x));

// Test basic composition
let aT = composeTypes(idType, idType, A);
def getA(x: aT): Integer = x.a;  // Should work via reduction
```

#### Success Criteria

1. Nested function applications are properly reduced during type checking
2. Field access on types produced by composed functions works correctly
3. The original function applications are preserved in elaborated results
4. No "cells not covered by any propagator" errors occur during zonking

## 4. Testing Strategy

### 4.1 Create Specialized Tests for Dependent Types

Implement tests that verify:
- Function types with type dependencies
- Equality of types with different variable names but same structure
- Union and intersection types with alpha-equivalent components

### 4.2 Union Type Testing Cases

Current test cases that need to be fixed:

```chester
// Widening (Success)
def f(x: Integer): Integer | String = x;
f(42);

// Subtyping (Success)
def g(x: Integer | String): Integer | String = x;
let x: Integer = 42;
let y: Integer | String = g(x);

// Invalid Subtyping (Failure - should be detected as error)
def f(x: Integer | String): Integer = x;
```

## 5. Implementation Steps

### 5.1 Phase 1: Core Type System Improvements
- [x] Document current type system architecture
- [x] Fix cell coverage issues in union type subtyping
  - [x] Implement helper method for cell coverage
  - [x] Ensure all union components are covered
  - [x] Fix early returns that leave cells uncovered
  - [ ] Fix remaining edge cases in union-subtype.chester (in progress)
- [ ] Implement type-level function application enhancements
- [ ] Add test cases for complex type-level functions

### 5.2 Phase 2: Advanced Type Features
- [ ] Test with complex type-level function examples
- [ ] Verify all success criteria are met
- [ ] Add more test cases for edge cases
- [ ] Document implementation details and usage patterns

## 6. Design Principles to Follow

### 6.1 Term Preservation
- Keep original terms in elaborated results
- Never reduce during elaboration
- Only use reduction for type-level comparisons
- Preserve source structure for better error reporting

### 6.2 Reduction Strategy
- Only reduce during type equality checking
- Use `ReduceMode.TypeLevel` for these internal reductions
- Use proper reduction context from current context
- Never reflect internal reductions in output

### 6.3 Documentation
- Keep this document updated with implementation progress
- Document design decisions and trade-offs
- Maintain clear test cases for each feature

## 7. Success Criteria

1. All tests pass, including the specialized dependent type tests
2. The type checking system correctly handles:
   - Complex dependent type scenarios
   - Intersection type comparisons
   - Union type subtyping
   - Type-level function applications
3. Documentation clearly explains dependent type concepts and usage patterns
4. Meta variables in complex types resolve correctly 

## 8. Running Tests

To run the tests for the type checker specifically, use the following SBT command:

```bash
# Run the FilesTyckTest suite to test the type checking system
sbt "rootJVM/testOnly chester.tyck.FilesTyckTest"
```

⚠️ **IMPORTANT WARNING**: Do NOT use the `-z` test filter option (e.g., `sbt "rootJVM/testOnly -- -z pattern"`) as it is broken and produces unreliable results. Instead, run the entire test suite and review the results.

## 9. Trait Implementation Plan

Basic trait implementation has been completed and documented in the development log (see `docs/src/dev/devlog.md` entry for 2025-03-19).

### Future Enhancements for Traits

While basic trait functionality is working, the following enhancements are planned for future implementation:

- Complete field requirement verification for traits
- Multiple trait inheritance support
- Trait method and default implementations
- More comprehensive trait test cases
- Advanced trait composition patterns 