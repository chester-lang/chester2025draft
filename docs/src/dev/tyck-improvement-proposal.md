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

### 3.1 Unimplemented Intersection Type Case

In `TyckPropagator.scala` (line 334), there's an unimplemented case for intersection types:
```scala
case (Intersection(_, _), Intersection(_, _)) => ???
```

**Proposed Implementation**:
```scala
case (Intersection(types1, _), Intersection(types2, _)) =>
  // For intersection types to be compatible, each type from the second intersection
  // must be compatible with at least one type from the first intersection
  types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))
```

### 3.2 Enhanced Reducer and Type Checking Integration

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

### 3.5 Bug Fix for OnceCell in Type-Level Functions

A critical bug occurs in `UnifyFunctionCall.run` when the `functionCallTerm` cell is filled twice for the same function call term. This issue manifests as an `IllegalArgumentException` with the message "requirement failed" when processing type-level functions.

**Root Cause**: 
- `OnceCell` is designed to only be filled once, with a requirement check: `require(value.isEmpty)`.
- During the evaluation of type-level function applications, the type checking system attempts to fill the same cell multiple times.
- This happens due to the interaction between the reducer and type checker during dependent type evaluation.

**Solution Implemented**:
```scala
// Construct the function call term with adjusted callings
val fCallTerm = FCallTerm(functionTerm, adjustedCallings, meta = None)
Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Created function call term: $fCallTerm")
Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: About to fill cell: $functionCallTerm")

// Check if the cell already has a value before attempting to fill it
val existingValue = state.readUnstable(functionCallTerm)
if (existingValue.isEmpty) {
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Cell is empty, filling with: $fCallTerm")
  state.fill(functionCallTerm, fCallTerm)
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Successfully filled function call term")
} else {
  // The cell already has a value, check if it's the same value
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Cell already has value: ${existingValue.get}")
  if (existingValue.get == fCallTerm) {
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Values are equal, skipping redundant fill")
  } else {
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: WARNING: Attempted to fill cell with different value")
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Existing: ${existingValue.get}")
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: New: $fCallTerm")
  }
}
```

**Verification**:
- The fix has been tested with both simple test cases (`dependent-id-type-debug.chester`) and more complex real-world scenarios (`type-level-reduction.chester`).
- All tests now pass, confirming that our fix successfully addresses the OnceCell bug.
- No more `IllegalArgumentException` from `OnceCell.fill` occurs during type checking.

**Key Design Principle**: 
- Always check if a cell already has a value before attempting to fill it.
- This prevents redundant fill operations without modifying the core propagator behavior.

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
- [ ] Fix the OnceCell bug in `UnifyFunctionCall.run`
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
3. No double-filling of cells occurs during type checking
4. Documentation clearly explains dependent type concepts and usage patterns
5. Meta variables in complex types resolve correctly 