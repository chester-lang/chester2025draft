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

The union type implementation has progressed with initial support for the pipe operator (`|`) syntax, but there are still challenges to resolve:

**Note**: The implementation of improved cell coverage logic and syntactic support for the pipe operator has been completed and documented in the devlog entry for 2025-03-15. Refer to `docs/src/dev/devlog.md` for details on completed improvements.

**🚧 Type Checking Issues**: While syntactically Union types can be parsed, there are currently issues with:
   - Type mismatches between `Integer` and `Integer | String`
   - Subtyping relationships not being properly resolved
   - Nested union types and generics not fully supported

**Next Steps**:
1. Fix the core subtyping mechanism to ensure consistent behavior:
   - Implement proper subtyping for `A <: A|B` relationships
   - Ensure consistent behavior in all contexts (function returns, assignments)
   - Fix cell coverage for all union type scenarios

2. Enhance parser and desalter support:
   - Improve handling of parenthesized union types
   - Add special handling for union types with generic parameters
   - Support deeper nested union types

3. Create a comprehensive test suite:
   - Simple union type assignments
   - Function return types with unions
   - Nested union types
   - Unions with generic type parameters

**Success Criteria**:
- All existing tests pass without errors
- New complex union type test cases pass
- No cell coverage errors occur
- Union types can be used seamlessly with other type constructs

### 3.2 Enhanced Type-Level Function Application Reduction

#### Current Limitation

The current type checker supports basic type-level function applications, but has limited handling of nested or recursive function applications in type-level contexts. When complex type-level expressions involve multiple nested function calls, the reducer may not properly evaluate them during type checking, leading to:

1. Type errors due to incomplete reduction of nested function applications
2. Reduced flexibility when using type-level functions in complex ways
3. Unclear error messages when type-level function applications fail

#### Implementation Plan

The implementation requires focused changes to:

1. **DefaultReducer Enhancement**:
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

Completed test cases:

```chester
// Widening (Success)
def f(x: Integer): Integer | String = x;
f(42);
```

Additional test cases needed:

```chester
// Nested Union Types
def complex(x: (Integer | String) | Boolean): (Integer | String) | Boolean = x;

// Union with parametric types
def generic<T>(x: T | String): T | String = x;

// Intersection with union
def mixed(x: (A & B) | C): (A & B) | C = x;
```

## 5. Implementation Steps

### 5.1 Phase 1: Core Type System Improvements

**Note**: Many of the planned improvements have been completed and documented in the devlog entries. Refer to `docs/src/dev/devlog.md` for detailed implementation notes, particularly the entries for 2025-03-15 (Type System Improvements) and 2025-03-23 (Comprehensive Type System Improvements Summary).

Remaining items to complete:
- [ ] Add more complex test cases for union types
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

## 8.5 Quick Test Guide

### Basic Test Cases By Feature

Below are minimal test cases for each feature that can be used for quick verification. Create these in separate `.chester` files in the `tests/tyck/` directory to validate implementation.

#### 1. Union Type Tests

```chester
// FILE: union_basic.chester
// Tests basic union type assignment
let a: Integer | String = 42;
let b: Integer | String = "hello";
```

```chester
// FILE: union_function.chester
// Tests function with union type parameters and return
def identity(x: Integer | String): Integer | String = x;
let a = identity(42);
let b = identity("hello");
```

```chester
// FILE: union_specific.chester
// Tests union-to-specific subtyping
def expectNumber(x: Number): Number = x;
let a: Integer | Float = 42;
expectNumber(a);  // Should succeed if Integer|Float <: Number
```

#### 2. Trait Tests

```chester
// FILE: trait_basic.chester
// Tests basic trait implementation
trait Showable {
  def show: String;
}

record Person(name: String, age: Integer) <: Showable {
  def show: String = name;
}

let p = Person("Alice", 30);
let s: Showable = p;
let name = s.show;
```

```chester
// FILE: trait_field.chester
// Tests trait field access
trait HasName {
  def name: String;
}

record User(name: String, email: String) <: HasName;

def getName(x: HasName): String = x.name;
let u = User("Bob", "bob@example.com");
getName(u);
```

#### 3. Type-Level Function Tests

```chester
// FILE: type_function.chester
// Tests type-level function application
def idType(x: Type): Type = x;

record Point(x: Integer, y: Integer);
let pointType = idType(Point);

def makePoint(p: pointType): pointType = p;
let p = Point(1, 2);
makePoint(p);
```

### Running Individual Test Cases

To run a specific test file:

```bash
# Compile and run a specific test file
sbt "rootJVM/testOnly chester.tyck.FilesTyckTest -- -only add.chester"
```

### Test Verification Checklist

For each test case, verify:

1. ✅ Compilation succeeds without errors
2. ✅ Type checks pass correctly
3. ✅ No "cells not covered by any propagator" errors
4. ✅ Error messages are clear and helpful when intentional errors are introduced

## 9. Trait Implementation Plan

**Note**: Basic trait implementation has been completed and documented in the development log. See the devlog entry for 2025-03-19 for implementation details and the entry for 2025-03-25 for enhanced trait implementation details.

### Future Enhancements for Traits

While basic trait functionality is working, the following enhancements are planned for future implementation:

- Complete field requirement verification for traits
- Multiple trait inheritance support
- Trait method and default implementations
- More comprehensive trait test cases
- Advanced trait composition patterns

## 10. Detailed Plan for Complex Union Types

**Note**: Many of the planned union type improvements have been completed and documented in the devlog entries for 2025-03-15 and 2025-03-23. The implementation includes proper handling of union subtyping and cell coverage.

### Future Work for Complex Union Types

To further extend support for complex union types (nested unions, unions with generic types), the following detailed technical steps are still needed:

1. **Enhance Parser and Desalter Support**:
   - Improve handling of parenthesized union types
   - Add special handling for union types with generic parameters
   - Support deeper nested union types

2. **Strengthen Type Checking for Advanced Cases**:
   - Enhance handling of nested unions by "flattening" when appropriate
   - Improve error reporting for complex union scenarios
   - Update unification to handle multi-level nested unions properly

3. **Expand the Test Suite**:
   - Add specific unit tests for each subtyping scenario
   - Test nested unions with different depths
   - Test unions with different combinations of types
   - Test function types with union parameters and return types
   - Test generic type parameters with union bounds
   - Test more complex scenarios combining unions, intersections, and generic types

### Success Metrics

The implementation will be considered successful when:

1. All test cases pass without errors
2. The type checker correctly handles complex nested unions
3. No "cells not covered" exceptions occur
4. Type error messages are clear and help identify the issue
5. The implementation can scale to handle arbitrarily complex union types