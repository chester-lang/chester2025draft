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

**Note**: Many key improvements have been completed and documented in the devlog entries. Refer to `docs/src/dev/devlog.md` for details on completed improvements, particularly those related to type system enhancements, trait implementation, and the new AutoConnect propagator.

## 3. Remaining Issues and Implementation Plan

### 3.1 ✅ Union Type Subtyping Implementation

**Status: COMPLETED**

The union type implementation has been completed with full support for the pipe operator (`|`) syntax and proper subtyping relationships. All components have been implemented as documented in the devlog.

Completed implementations include:
- Union-to-Union subtyping (`A|B <: C|D`)
- Specific-to-Union subtyping (`A <: B|C`)
- Union-to-Specific subtyping (`A|B <: C`)
- Cell coverage mechanisms for union types
- Proper type checking for union types in all contexts

### 3.2 ✅ Removal of the EnsureCellCoverage Hack

**Status: COMPLETED**

The `EnsureCellCoverage` hack has been replaced with a proper `AutoConnect` propagator that establishes meaningful type relationships. See the devlog for implementation details.

Key improvements include:
- Analysis of term structure to create proper type connections
- Smart handling of union and intersection types
- Specialized support for function calls and their arguments
- Default value support for truly unconstrained type variables
- Complete removal of all `EnsureCellCoverage` instances

All success criteria have been met:
- All cells are covered by meaningful propagators with actual logic
- No explicit `EnsureCellCoverage` instances in the codebase
- Union types and other complex types work correctly
- Type errors are detected and reported accurately
- Tests pass with proper constraint checking
- Improved code clarity and maintainability

### 3.3 Enhanced Type-Level Function Application Reduction

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
   - Further enhance the `tryUnify` method to handle complex function call terms
   - Ensure proper cell coverage for nested function applications
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

**Status**: Most core improvements have been completed.

Completed improvements (see devlog for details):
- ✅ Enhanced Type Structure Reduction in DefaultReducer
- ✅ Alpha-Equivalence Checking in TyckPropagator
- ✅ Enhanced Type Level Comparison
- ✅ Cell Coverage Mechanisms with AutoConnect propagator
- ✅ Union Type Subtyping Implementation
- ✅ Basic Trait Implementation

Remaining items to complete:
- [ ] Add more complex test cases for nested union types
- [ ] Complete enhanced type-level function application for complex nested cases
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

## 9. ✅ Trait Implementation Plan

**Status: BASIC IMPLEMENTATION COMPLETED**

Basic trait implementation has been completed and documented in the development log. See the devlog for implementation details.

### Future Enhancements for Traits

While basic trait functionality is working, the following enhancements are planned for future implementation:

- Complete field requirement verification for traits
- Multiple trait inheritance support
- Trait method and default implementations
- More comprehensive trait test cases
- Advanced trait composition patterns

## 10. ✅ Detailed Plan for Complex Union Types

**Status: BASIC IMPLEMENTATION COMPLETED**

Many of the planned union type improvements have been completed and documented in the devlog. The implementation includes proper handling of union subtyping and cell coverage through the new AutoConnect propagator.

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

## 11. Experimental Implementation Notes: Union Type Improvements

During an experimental implementation attempt to fix union type handling, the following specific changes were made to improve union type subtyping and prevent cell coverage errors:

### 11.1 Union-to-Specific Type Relationship Changes

The key issue identified was in how union types were converted to specific types (e.g., `Integer | String` to `Integer`):

**Original Implementation Issues:**
- In `TyckPropagator.scala`, the `unionSpecificCompatible` method only checked if ANY component of the union was compatible with the specific type
- This allowed unsound expressions like `def f(x: Integer | String): Integer = x` to type-check

**Specific Changes Made:**
- Modified `unionSpecificCompatible` method to check if ALL union components are compatible with the specific type
- Changed the implementation logic from `unionTypes.exists(compatible)` to `!unionTypes.exists(!compatible)`
- Added explicit error reporting in `handleUnionSpecific` method within the `Unify` case class
- Enhanced debug output to include step-by-step component compatibility checks
- Modified `Elaborater.unify` method's union case to properly handle specific-to-union compatibility

### 11.2 DefaultValuePropagator Implementation

To solve the "cells not covered by any propagator" errors:

**Specific Implementation Changes:**
- Rewritten `ensureDefaultValue[T]` method (line ~1087 in `TyckPropagator.scala`):
  - Added explicit `state.addPropagator` call to add a dedicated propagator
  - Added explicit `state.fill` call to ensure cells have values
  - Added diagnostic logging to track cell state

- Created a new `DefaultValuePropagator[T]` case class with:
  - Set `score = 100` to give it high priority
  - Implemented `run`, `defaulting`, and `naiveFallbackZonk` methods
  - Added proper cell tracking with `readingCells`, `writingCells`, and `defaultingCells`
  - Added custom `identify` method returning `Some(id)` to prevent propagator removal

### 11.3 Infinite Recursion Prevention

Specific changes to break cyclic dependencies:

- In `UnionOf` propagator:
  - Removed recursive calls to `unify` that could cause infinite loops
  - Used early returns and simplified value checking logic
  - Added guard conditions before recursive propagator creation

- In the `handleUnionSpecific` method:
  - Used `ensureDefaultValue` for each union component instead of creating linked propagators
  - Added filtered component selection before creating propagator connections
  - Used explicit value discarding with statements like `val _ = ensureDefaultValue(unionType)`

### 11.4 Test File Updates

Specific file changes:
- Moved `tests/tyck-fails/union-subtype-fail.chester.todo` to active status by removing `.todo` extension
- Updated the expected behavior of `def f(x: Integer | String): Integer = x` to correctly fail
- Modified `tests/tyck/simple-union.chester.todo` to ensure it properly tests union widening

### 11.5 Key Identifier Changes for Future Reference

The most significant method and identifier changes were:
- `unionSpecificCompatible`: Changed compatibility check logic
- `handleUnionSpecific`: Rewrote to handle ALL union components
- `ensureDefaultValue`: Enhanced with propagator creation
- Added new `DefaultValuePropagator` class
- Modified `Unify.defaulting` case for union handling
- Updated union component debug logging using `Debug.debugPrint`

These changes collectively represent an approach to fixing union type subtyping that ensures sound type checking while preventing "cells not covered" errors through improved propagator management.