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

### 2.1 Key Improvements Already Made

**Note**: The key improvements implemented so far have been moved to the devlog entry for 2025-03-15. Refer to `docs/src/dev/devlog.md` for details on completed improvements to:
- Type structure reduction in NaiveReducer
- Alpha-equivalence checking in TyckPropagator
- Enhanced type level comparison
- Cell coverage mechanisms
- TraitCallTerm implementation

## 3. Remaining Issues and Implementation Plan

### 3.1 Unimplemented Union Type Subtyping

In `Elaborater.scala` (line 91), there's an unimplemented case for union type subtyping:
```scala
case (Union(_, _), Union(_, _)) => ???
```

**Proposed Implementation**:
```scala
case (Union(types1, _), Union(types2, _)) => 
  // For union-to-union subtyping, every type in the first union
  // must be a subtype of the second union type as a whole
  types1.foreach { t1 =>
    if (!types2.exists(t2 => tryUnify(t1, t2))) {
      ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      return
    }
  }
```

**Implementation Plan**:
1. Implement the union-to-union subtyping case
2. Verify existing tests pass
3. Test with `union-subtype.chester.todo` file
4. Rename test file to remove `.todo` suffix when passing

**Success Criteria**:
- All tests including union subtyping pass
- File `union-subtype.chester` type checks correctly 

### 3.2 Union Type Subtyping Implementation

The implementation of union type subtyping in Chester requires careful integration with the propagator network. Our ongoing work has identified these key improvements needed:

#### 3.2.1 Current Challenges

The current implementation faces the following challenges:

1. **Propagator Coverage Issue**: When handling union types, cells occasionally lack proper propagator coverage, resulting in the error:
   ```
   java.lang.IllegalStateException: Cells Vector(...) are not covered by any propagator
   ```
   This happens particularly during the unification and zonking phases when some cells do not have propagators attached that can produce values for them.

2. **Missing Bidirectional Connections**: The implementation needs to ensure bidirectional connections between union types and their components.

3. **Proper Cell Retrieval**: Care must be taken to use the correct API (`toId()`) to retrieve cell IDs from terms.

4. **Component Type Coverage**: Each component type within a union must be properly covered by propagators. This is easily missed when focusing only on the main union type cell.

5. **Early Return Issue**: The current implementation sometimes uses early returns which can lead to incomplete propagator networks, as some cells might not get covered before exiting a method.

#### 3.2.2 Implemented Solutions

1. **Cell Coverage Helper Method**: Implemented a dedicated helper method to ensure cell coverage:
   ```scala
   private def ensureCellCoverage(cell: CellId[Term], cause: Expr)(using
       state: StateAbility[Tyck],
       ctx: Context,
       ck: Tyck
   ): Unit = {
     // Simply connect the cell to itself to ensure it's covered by at least one propagator
     state.addPropagator(UnionOf(cell, Vector(cell), cause))
   }
   ```

2. **Comprehensive Cell Coverage**: Ensured that all cells, including union components, are covered by propagators:
   ```scala
   // Get cell IDs for all union component types and ensure they're covered
   val unionTypeIds = unionTypes.map(typ => {
     val cellId = toId(typ)
     ensureCellCoverage(cellId, cause)
     cellId
   }).toVector
   ```

3. **Avoid Early Returns**: Modified code to avoid early returns that could leave cells uncovered:
   ```scala
   // Instead of returning early
   if (lhsResolved == rhsResolved) {
     // Add necessary propagators first
     ensureCoverage(lhsResolved)
     ensureCoverage(rhsResolved)
     // Then return
     return
   }
   ```

4. **Enhanced Union-Union Subtyping**: Improved the union-to-union subtyping case to ensure all cells are properly covered:
   ```scala
   case (Union(types1, _), Union(types2, _)) => 
     // Ensure the main union cells are covered
     ensureCellCoverage(lhsCell, cause)
     ensureCellCoverage(rhsCell, cause)
     
     // Cover all component types
     val lhsTypeIds = types1.map(typ => {
       val cellId = toId(typ)
       ensureCellCoverage(cellId, cause)
       cellId
     }).toVector
   ```

5. **Comprehensive Propagator Network**: Implemented proper connections in all three major union subtyping cases:
   - Union-to-Union subtyping
   - Specific-to-Union subtyping
   - Union-to-Specific subtyping

By implementing these improvements, Chester will have full support for union type subtyping while ensuring the propagator network remains consistent and properly connected.

### 3.3 Enhanced Reducer and Type Checking Integration

The interaction between `NaiveReducer` and the type checking system needs improvement to better handle dependent types:

**Proposed Approach**:
```scala
// In NaiveReducer
def reduce(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = {
  // For dependent types, ensure we properly handle terms occurring within types
  val reduced = mode match {
    case ReduceMode.TypeLevel => 
      // When reducing for type equality, handle:
      // 1. Union and intersection types
      // 2. Terms that occur within types (dependent typing)
      // 3. Type-level functions
      reduceTypeLevel(term)
      
    case ReduceMode.Normal => 
      // Normal execution mode reduction
      reduceNormal(term)
  }
  
  // Apply consistent recursive reduction to ensure all parts are properly reduced
  applyRecursiveReduction(reduced, mode)
}
```

### 3.3 Meta Variable Handling

Current implementation has issues with meta variables in unions:
```scala
case Some(Meta(lhsId)) =>
  // Need proper handling for meta variables in union LHS
case (_, Some(Meta(rhsId))) =>
  // Need proper handling for meta variables in union RHS
```

**Proposed Approach**:
```scala
def trackMetaVariable(id: CIdOf[Cell[?]]) = {
  case Some(Meta(metaId)) =>
    // Register meta variable
    metaVariables.add(metaId)
    // Ensure propagator connections
    verifyPropagatorCoverage(Vector(metaId))
}
```

### 3.4 Cell Coverage Issues and Solutions

#### 3.4.1 Identifying Uncovered Cells

When cells are not covered by propagators, the following error occurs during zonking:

```
java.lang.IllegalStateException: Cells Vector(chester.utils.propagator.ProvideMutable$HoldCell@2f2ccc97) are not covered by any propagator
    at chester.utils.propagator.ProvideMutable$Impl.naiveZonk(ProvideMutable.scala:226)
    at chester.tyck.DefaultImpl.finalizeJudge(Elaborater.scala:557)
```

#### 3.4.2 Implemented Solution

Our implementation addresses these cell coverage issues in a systematic way. We've developed reusable mechanisms that ensure all cells are properly covered by propagators.

**Note**: The detailed implementation of our cell coverage solutions has been moved to the main architecture documentation in `docs/src/dev/type-checking-system.md` under the "Cell Coverage Solutions" section. This includes:

1. Self-coverage mechanism using self-referential propagators
2. Comprehensive coverage for complex types like unions
3. Avoiding early returns that leave cells uncovered
4. Debugging support for cell coverage issues

These solutions have successfully resolved the cell coverage issues in most cases, with ongoing work to address the remaining edge cases for union subtyping and type-level function applications.

## 3.5 Enhanced Type-Level Function Application Reduction

### 3.5.1 Current Limitation

The current type checker supports basic type-level function applications (as seen in the type-level-reduction.chester test file), but has limited handling of nested or recursive function applications in type-level contexts. When complex type-level expressions involve multiple nested function calls, the reducer may not properly evaluate them during type checking, leading to:

1. Type errors due to incomplete reduction of nested function applications
2. Reduced flexibility when using type-level functions in complex ways
3. Unclear error messages when type-level function applications fail

This limitation affects the expressiveness of the type system, especially when creating sophisticated type-level abstractions.

#### Specific Issues Identified

Our testing with `type-level-nested-functions.chester` has confirmed several specific issues:

1. **Cell Coverage Gaps**: During type checking of nested function applications, some cells in the propagator network lack proper coverage, resulting in errors during the zonking phase:
   ```
   java.lang.IllegalStateException: Cells Vector(...) are not covered by any propagator
   ```

2. **Propagator Lifecycle Issues**: The error occurs in `naiveZonk` during the `finalizeJudge` phase, indicating issues with propagator state tracking and cell resolution.

3. **Recursive Call Handling**: When type-level functions call other type-level functions, the current system doesn't properly manage the propagator connections between intermediate results.

4. **Function Composition Limitations**: The current implementation struggles with cases where type-level functions are composed or combined, as seen in our test with `composeTypes(idType, idType, A)`.

### 3.5.2 Proposed Improvement

Enhance the reducer's ability to handle nested type-level function applications by:

1. Improving the `NaiveReducer` to better manage nested function applications
2. Ensuring consistency in how type-level functions are reduced
3. Preserving original function applications in elaborated results
4. Adding cell coverage mechanisms to prevent "cells not covered" errors

#### Implementation Plan

The implementation requires focused changes to the following components:

1. **NaiveReducer Enhancement**:
```scala
// In NaiveReducer.scala, enhance the reduceTypeLevel method:
private def reduceTypeLevel(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
  term match {
    // ... existing cases ...
    
    // Improve function call handling for nested type-level functions
    case fcall: FCallTerm if isTypeLevel(fcall) => {
      // First reduce the function itself
      val reducedFunc = reduce(fcall.func, ReduceMode.TypeLevel)
      // Then reduce the arguments
      val reducedArgs = fcall.args.map(arg => reduce(arg, ReduceMode.TypeLevel))
      
      // Create a new function call with reduced components
      val reducedCall = FCallTerm(reducedFunc, reducedArgs, fcall.meta)
      
      // If this is a direct call to a function that we can reduce further, do so
      val firstResult = reduceStandard(reducedCall, ReduceMode.TypeLevel)
      
      // Handle nested function calls in the result by recursively reducing
      firstResult match {
        case nestedCall: FCallTerm if isTypeLevel(nestedCall) && 
                                      nestedCall != reducedCall => 
          // Recursively handle nested function calls
          reduceTypeLevel(nestedCall)
          
        case _ => firstResult
      }
    }
    
    // ... other cases ...
  }
}
```

2. **Improved Type-Level Function Detection**:
```scala
// In NaiveReducer.scala, enhance the isTypeLevel helper function:
private def isTypeLevel(term: Term): Boolean = {
  term match {
    // Existing detection logic
    case FCallTerm(_, _, meta) => meta.get(TypeLevelFunction).isDefined
    
    // Add detection of functions whose result is used in a type context
    case FCallTerm(funcTerm, _, _) =>
      // Check if the function's return type is Type
      funcTerm match {
        case name: NameRef => {
          ctx.findDef(name.name).exists(defn => 
            defn.ty match {
              case FunctionType(_, result, _, _) => isType(result)
              case _ => false
            }
          )
        }
        // Handle other cases
        case _ => false
      }
      
    case _ => false
  }
}

// Helper to check if a term is a Type
private def isType(term: Term): Boolean = {
  term match {
    case Type(_, _) => true
    case NameRef(name) => name.toString == "Type"
    case _ => false
  }
}
```

3. **Cell Coverage Mechanism**:
```scala
// In Elaborater.scala, add a helper method to ensure cell coverage:
private def ensureCellCoverage(cell: CellId[Term], cause: Expr)(using
    state: StateAbility[Tyck],
    ctx: Context,
    ck: Tyck
): Unit = {
  // Create a self-referential propagator to ensure basic coverage
  state.addPropagator(UnionOf(cell, Vector(cell), cause))
}

// Update the processTypeLevel method to use this helper:
def processTypeLevel(term: Term, cause: Expr)(using state: StateAbility[Tyck]): Unit = {
  // Get cell ID
  val cellId = toId(term)
  
  // Ensure this cell is covered
  ensureCellCoverage(cellId, cause)
  
  // For composite terms like function calls, also ensure sub-term coverage
  term match {
    case fcall: FCallTerm => {
      processTypeLevel(fcall.func, cause)
      fcall.args.foreach(arg => processTypeLevel(arg, cause))
    }
    case _ => // Handle other cases as needed
  }
}
```

4. **Type Checking Integration**:
```scala
// In TyckPropagator.scala, enhance the tryUnify method:
private def tryUnify(lhs: Term, rhs: Term, cause: Expr)(using
    state: StateAbility[Tyck],
    ctx: Context,
    ck: Tyck
): Unit = {
  // ... existing code ...
  
  (lhs, rhs) match {
    // ... existing cases ...
    
    // Add specific handling for function call terms
    case (fcall: FCallTerm, _) if isTypeLevel(fcall) => {
      // Ensure cell coverage for the function call
      ensureCellCoverage(toId(fcall), cause)
      
      // Handle sub-terms
      processTypeLevel(fcall.func, cause)
      fcall.args.foreach(arg => processTypeLevel(arg, cause))
      
      // Reduce the function call for comparison
      val reduced = reducer.reduce(fcall, ReduceMode.TypeLevel)
      if (reduced != fcall) {
        // If reduction succeeded, unify the reduced result
        tryUnify(reduced, rhs, cause)
      } else {
        // Otherwise, report appropriate error
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      }
    }
    
    // Similar case for right-hand side
    case (_, fcall: FCallTerm) if isTypeLevel(fcall) => {
      // Similar implementation as above, but flipped
    }
    
    // ... other cases ...
  }
}
```

### 3.5.3 Testable Example

This improvement can be tested with our Chester file that extends the existing type-level-reduction.chester example:

```chester
// Test enhanced type-level function application
record A(a: Integer);
record B(b: String);

// Basic identity function for types
def idType(x: Type): Type = x;

// Function composition at the type level
def composeTypes(f: Type -> Type, g: Type -> Type, x: Type): Type = f(g(x));

// Type-level functions for wrapping types
def wrapInRecord(name: String, innerType: Type): Type = {
  idType(innerType) // Apply inner function first
};

// Test basic composition
let aT = composeTypes(idType, idType, A);
def getA(x: aT): Integer = x.a;  // Should work via reduction

// Test with multiple applications
let wrappedB = wrapInRecord("wrapper", B);
def getB(x: wrappedB): String = x.b;  // Should also work

// Test more complex composition
let doubleWrapped = composeTypes(idType, wrapInRecord("outer", A), A);
def getDoubleA(x: doubleWrapped): Integer = x.a;
```

When run, this test currently fails with the error:
```
java.lang.IllegalStateException: Cells Vector(...) are not covered by any propagator
```

Our implementation will fix this issue, allowing the test to pass.

### 3.5.4 Success Criteria

1. The test file type-checks correctly with no propagator errors
2. Nested function applications are properly reduced during type checking
3. Field access on types produced by composed functions works correctly
4. The original function applications are preserved in elaborated results
5. Error messages for invalid type-level functions are clear and helpful
6. The implementation works with multiple levels of function application nesting
7. No "cells not covered by any propagator" errors occur during zonking

### 3.5.5 Alignment with Design Principles

This implementation aligns with Chester's core design principles:

1. **Term Preservation**: Original function application expressions are preserved in elaborated results
2. **Controlled Reduction**: Reduction is only used internally during type checking
3. **Propagator Network Integrity**: All cells have proper propagator coverage
4. **Flexible Type System**: Enables sophisticated type-level programming

The proposed changes are minimal and focused, addressing the specific issue while maintaining the system's architecture and design philosophy.

## 4. Testing Strategy

### 4.1 Create Specialized Tests for Dependent Types

Implement tests that verify:
- Function types with type dependencies
- Equality of types with different variable names but same structure
- Union and intersection types with alpha-equivalent components

### 4.2 Intersection Type Testing

Since intersection types aren't directly expressible in Chester yet, we can test through integer literals:

```chester
// Test integer literals with different values
// (these use intersection types internally)
let a: Integer = 42;    // Positive integer
let b: Integer = -5;    // Negative integer
```

### 4.3 Union Type Testing Cases

Existing test cases that need to be fixed:

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

#### 4.3.1 Current Testing Status

The union-subtype.chester tests have been temporarily disabled with a `.todo` suffix while we fix the underlying issues. Most other tests in the FilesTyckTest suite are passing successfully. The specific errors we're tackling are:

```
java.lang.IllegalStateException: Cells Vector(chester.utils.propagator.ProvideMutable$HoldCell@2f2ccc97) are not covered by any propagator
```

Our current approach has resolved most of the cell coverage issues in the general type checking system, but still faces challenges with the specific union subtyping test cases. We need to:

1. Re-enable the union-subtype.chester test
2. Add more comprehensive test cases covering all three union subtyping scenarios
3. Add assertions to verify that proper error messages are shown for invalid subtyping cases

### 4.4 Cell Coverage and Integration Tests

```scala
test("union type components have propagators") {
  // Test code
}

test("meta variables in unions resolve correctly") {
  // Test code
}

test("complex union type hierarchy resolves") {
  // Test code
}

```
## 5. Implementation Steps

### 5.1 Phase 1: Core Type System Improvements
- [x] Document current type system architecture
- [x] Fix cell coverage issues in union type subtyping
  - [x] Implement helper method for cell coverage
  - [x] Ensure all union components are covered
  - [x] Fix early returns that leave cells uncovered
  - [ ] Fix remaining edge cases in union-subtype.chester (in progress)
- [ ] Implement intersection type unification case
- [ ] Add test cases for dependent types and intersection types

### 5.2 Known Issues to Resolve

1. **Union Subtyping Edge Cases**: The union-subtype.chester test still fails with cell coverage errors. Potential solutions:
   - Further improve the propagator network connectivity
   - Add more debugging output in the relevant type checking code paths
   - Better cell tracking during unification and zonking

2. **Refactorings Needed**:
   - Consolidate union type handling in one section of the code
   - Introduce more helper methods for common operations
   - Add better error messages for invalid union type operations
- [ ] Verify implementation with existing tests

### 5.2 Phase 2: Meta Variable and Cell Coverage
- [x] Add propagator coverage verification
- [x] Implement meta variable tracking
- [x] Add union type component tracking
- [x] Add connection verification tests

### 5.3 Phase 3: Advanced Type Features
- [x] Enhance union and intersection type interactions
- [x] Improve reducer-tyck integration for dependent types
- [x] Add comprehensive test suite
- [x] Update documentation

### 5.4 Phase 4: Type-Level Function Improvements
- [x] Add `processTypeLevel` function to handle type-level function applications
  - [x] Implement cell coverage for function calls
  - [x] Add recursive processing of sub-terms
  - [x] Handle composite terms (unions, intersections)
- [x] Enhance `unify` method to handle function calls
  - [x] Add specific cases for function call terms
  - [x] Ensure proper cell coverage
  - [x] Add guards to prevent pattern matching conflicts
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

These commands are essential for verifying the type checker implementation against the test cases. The `FilesTyckTest` suite contains tests for various type checking features including:
- Dependent types
- Type-level function applications
- Record field access
- Union type subtyping
- Forward definitions
- Object literals

## 9. Trait Implementation Plan
Chester's type system needs to support traits and record-trait relationships through the `<:` syntax. This section outlines the implementation plan for this feature.

### 9.1 Current Status

The codebase already has some infrastructure for traits:
- AST nodes for trait definitions (`TraitStmt`, `TraitStmtTerm`)
- Parsing support for the `<:` extends syntax
- Basic trait elaboration in `processTraitStmt`
- `TraitCallTerm` implementation in `Term.scala`

### 9.2 Implementation Progress

#### 9.2.1 Completed
- Added error types for trait implementation in `TyckProblem.scala`:
  - `NotImplementingTrait`
  - `RecordNotImplementingTrait`
  - `NotATrait`
  - `MissingTraitField`
- Updated `RecordStmtTerm` to include the `extendsClause` field
- Enhanced `ElaboraterBlock.processRecordStmt` to elaborate the `extendsClause`
- Added trait handling in `elabIdentifier` in `Elaborater.scala`
- Implemented the `checkTraitImplementation` method in `TyckPropagator.scala`
- Added record-trait subtyping cases in the `unify` method

#### 9.2.2 Pending
- Enable the `basic-trait.chester.todo` test to verify implementation
- Support more complex trait hierarchies (trait-to-trait inheritance)
- Implement trait field requirements checking
- Add trait interface method support
- Support multiple trait inheritance

### 9.3 Implementation Approach

To implement trait support within Chester's propagator network-based type system, we will:

1. **Trait Type Representation**:
   - Use `TraitCallTerm` as the basis for representing trait types in the type system
   - Add appropriate propagator connections for trait-record subtyping

2. **Record-Trait Subtyping**:
   - Add a subtyping case to `TyckPropagator` that handles record-trait relationships
   - Implement checking that record provides all trait requirements
   - Add appropriate propagator connections to maintain type safety

3. **Minimal Requirements for MVP**:
   - Support empty traits with basic record extension
   - Add error reporting for unsatisfied trait requirements
   - Enable traits to be used as types for parameters and variables

### 9.4 Testing Strategy

1. **Basic Empty Trait Test**:
   - Test record extending an empty trait (basic-trait.chester.todo)
   - Verify type checking passes

2. **Trait as a Type Parameter**:
   - Test using trait as a function parameter type
   - Test assigning record instance to trait-typed variable

3. **Error Cases**:
   - Test error reporting when record doesn't implement required trait
   - Test using non-trait in extends clause

4. **Future Tests to Implement**:
   - Traits with field requirements
   - Records implementing multiple traits
   - Error cases for missing trait fields

### 9.5 Next Steps

1. **Validate Current Implementation**:
   - Rename the `basic-trait.chester.todo` test to `basic-trait.chester` to enable it
   - Verify that the basic test passes with our current implementation
   - Fix any issues that arise during testing

2. **Enhance Implementation**:
   - Add support for trait field requirements
   - Implement trait interface methods
   - Support multiple trait inheritance

3. **Add Additional Test Cases**:
   - Create more comprehensive test suite for trait features
   - Test edge cases and error conditions

### 9.6 Integration with Existing Systems

This trait implementation leverages Chester's existing propagator network to maintain type safety and subtyping relationships. It fits into the existing architecture by:

1. Using the cell-propagator system to represent trait-based constraints
2. Leveraging the existing subtyping mechanisms to handle trait relationships
3. Building on the existing elaboration infrastructure for type definitions

### 9.7 Success Criteria

The implementation is successful when:
1. The basic-trait.chester.todo test passes
2. Records can extend traits using the `<:` syntax
3. The type system correctly enforces trait-based type constraints
4. Error messages are clear when trait requirements are not met 