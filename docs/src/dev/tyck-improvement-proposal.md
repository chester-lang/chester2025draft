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
- Enhanced the `reduceTypeStructure` method in `NaiveReducer` to properly handle complex type structures
- Fixed the `areAlphaEquivalent` method in `TyckPropagator` to properly fall back to equality checking
- Improved handling of `TelescopeTerm` objects in function types for alpha-equivalence checks
- Added bound variable tracking in alpha-equivalence to better handle dependent types
- Improved documentation of key methods
- Enhanced record field access to properly handle dependent types

### 2.2 Completed Improvements

#### 2.2.1 Enhanced Type Structure Reduction in NaiveReducer
The `reduceTypeStructure` method properly handles:
- Union types by recursively reducing their component types
- Intersection types with proper reduction
- Type-level function applications with recursive reduction for complex result types

```scala
private def reduceTypeStructure(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
  term match {
    case Union(types, meta) =>
      Union(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
      
    case Intersection(types, meta) =>
      Intersection(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
    
    case fcall: FCallTerm if isTypeLevel(fcall) =>
      // First reduce normally
      val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)
      // Then check if the result needs further type structure handling
      reduced match {
        case Union(_, _) | Intersection(_, _) => reduceTypeStructure(reduced)
        case _ => reduced
      }
      
    case _ => term
  }
}
```

#### 2.2.2 Alpha-Equivalence Checking in TyckPropagator
The `areAlphaEquivalent` method has been enhanced to:
- Properly handle function types with bound variables
- Compare union and intersection types correctly
- Fall back to regular equality for other cases

```scala
private def areAlphaEquivalent(lhs: Term, rhs: Term)(using state: StateAbility[Tyck], localCtx: Context): Boolean = {
  (lhs, rhs) match {
    case (FunctionType(params1, result1, effects1, _), FunctionType(params2, result2, effects2, _)) =>
      if (params1.length != params2.length || effects1 != effects2) false
      else {
        val paramsEqual = params1.zip(params2).forall { case (p1, p2) =>
          // Check telescope parameters are equivalent
          if (p1.args.length != p2.args.length) false
          else {
            p1.args.zip(p2.args).forall { case (arg1, arg2) =>
              areAlphaEquivalent(arg1.ty, arg2.ty)
            }
          }
        }
        
        if (!paramsEqual) false
        else {
          // For the result type, need to consider bindings
          areAlphaEquivalent(result1, result2)
        }
      }
      
    case (Union(types1, _), Union(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    case (Intersection(types1, _), Intersection(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    // For other cases, fall back to regular equality check
    case _ => lhs == rhs  // Changed from returning false
  }
}
```

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
   java.lang.IllegalStateException: Cells are not covered by any propagator
   ```

2. **Missing Bidirectional Connections**: The implementation needs to ensure bidirectional connections between union types and their components.

3. **Proper Cell Retrieval**: Care must be taken to use the correct API (`toId()`) to retrieve cell IDs from terms.

#### 3.2.2 Proposed Implementation Approach

1. **Utilize Existing Propagators**: Leverage the `UnionOf` propagator specifically designed for union types:
   ```scala
   // For each union component
   unionTypeIds.foreach { componentId => 
     state.addPropagator(UnionOf(targetTypeId, unionComponentIds, cause))
   }
   ```

2. **Ensure Bidirectional Connections**: For union-to-union subtyping, create connections in both directions:
   - Connect left union components to right union
   - Connect right union components to left union

3. **Handle Edge Cases**:
   - Empty union components (ensure lists are non-empty)
   - Specific-to-union connections with compatibility checks
   - Union-to-specific connections with all components validation

4. **Comprehensive Testing**: Ensure all tests in `union-subtype.chester` pass, covering:
   - Function parameters accepting union types
   - Function returns with union types
   - Direct union type assignments

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

### 3.4 Cell Coverage Verification

```scala
def verifyPropagatorCoverage(cells: Vector[CIdOf[Cell[?]]]) = {
  cells.foreach { cell =>
    require(
      cell.zonkingPropagators.nonEmpty || 
      isFullyResolved(cell),
      s"Cell $cell has no zonking propagators"
    )
  }
}
```

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

Existing test cases:

```chester
// Widening (Success)
def f(x: Integer): Integer | String = x;
f(42);

// Subtyping (Success)
def g(x: Integer | String): Integer | String = x;
let x: Integer = 42;
let y: Integer | String = g(x);

// Invalid Subtyping (Failure)
def f(x: Integer | String): Integer = x;
```

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
- [ ] Implement intersection type unification case
- [ ] Add test cases for dependent types and intersection types
- [ ] Verify implementation with existing tests

### 5.2 Phase 2: Meta Variable and Cell Coverage
- [ ] Add propagator coverage verification
- [ ] Implement meta variable tracking
- [ ] Add union type component tracking
- [ ] Add connection verification tests

### 5.3 Phase 3: Advanced Type Features
- [ ] Enhance union and intersection type interactions
- [ ] Improve reducer-tyck integration for dependent types
- [ ] Add comprehensive test suite
- [ ] Update documentation

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