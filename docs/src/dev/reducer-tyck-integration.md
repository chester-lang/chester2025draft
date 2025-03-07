# Reducer and Type Checking Integration in a Dependent Type Setting

This document outlines a plan for improving the interaction between the `NaiveReducer` and the type checking system in Chester, specifically considering its dependent type system.

## Current State and Challenges

Currently, the type checking system in `TyckPropagator` uses `NaiveReducer` with `ReduceMode.TypeLevel` for type equality checking:

```scala
// In TyckPropagator.scala
val lhsResolved = readVar(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
val rhsResolved = readVar(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))
```

In a dependent type system like Chester's, this approach faces several challenges:

1. **Type/Term Interdependence**: Types can depend on terms, blurring the distinction between type-level and term-level reduction.
2. **Type Equality**: Equality checking becomes more complex since it must consider conversion between terms within types.
3. **Normalization Requirements**: Terms within types need proper normalization during comparison.
4. **Context Sensitivity**: Type reduction must respect the typing context.

## Proposed Improvement: Enhanced Reduction for Dependent Types

Instead of creating distinct reducers or specialized methods, we should refine the existing reduction approach to better handle the complexities of dependent types:

### 1. Consistent Application of Reduction Mode

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

// Helper method for type-level reduction
private def reduceTypeLevel(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
  term match {
    // Handle different term forms appropriately for type equality checking
    case Union(types, meta) =>
      Union(types.map(t => r.reduce(t, ReduceMode.TypeLevel)), meta)
      
    case Intersection(types, meta) =>
      Intersection(types.map(t => r.reduce(t, ReduceMode.TypeLevel)), meta)
      
    // Handle terms that appear in types (the essence of dependent typing)
    case FCallTerm(f, args, meta) if isTypeFunction(f) =>
      // Type functions must be fully evaluated for type equality
      fullyReduceTypeFunction(f, args, meta)
      
    // Other cases continue with standard reduction
    case _ => standardReduction(term)
  }
}
```

### 2. Refined Type Equality Checking in TyckPropagator

```scala
// In TyckPropagator.scala
def areTypesEqual(lhs: Term, rhs: Term)(using
    localCtx: Context,
    ck: Tyck,
    state: StateAbility[Tyck]
): Boolean = {
  given ReduceContext = localCtx.toReduceContext
  given Reducer = localCtx.given_Reducer
  
  // For dependent types, equality requires alpha-equivalence checking
  val lhsResolved = readVar(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
  val rhsResolved = readVar(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))
  
  // Alpha-equivalence check for dependent types
  areAlphaEquivalent(lhsResolved, rhsResolved)
}

// Helper method to check alpha-equivalence (crucial for dependent types)
private def areAlphaEquivalent(lhs: Term, rhs: Term): Boolean = {
  // This checks structural equality while respecting bound variables
  // which is essential for dependent type systems
  (lhs, rhs) match {
    // Special cases for types with internal structure
    case (Union(types1, _), Union(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    case (Intersection(types1, _), Intersection(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    // More cases for other type forms
    // ...
  }
}
```

## Implementation Steps

1. **Step 1**: Enhance `NaiveReducer` to better handle dependent types
   - Refine the reduction logic for both type-level and term-level contexts
   - Ensure terms within types are consistently reduced

2. **Step 2**: Improve type equality checking in `TyckPropagator`
   - Implement proper alpha-equivalence checking for types
   - Handle the structures of union and intersection types appropriately

3. **Step 3**: Strengthen context handling during reduction
   - Ensure variable bindings are respected during reduction
   - Maintain consistent contexts for dependent types

## Expected Benefits

1. **Type Soundness**: More robust type checking that respects dependent type theory.
2. **Consistent Behavior**: Uniform reduction behavior across the type system.
3. **Better Error Messages**: More precise type equality checking leads to better error reporting.
4. **Theoretical Correctness**: Adherence to dependent type theory principles.

## Testing Strategy

1. **Unit Tests**: Test reduction with various types including dependent function types.
2. **Integration Tests**: Verify type checking behavior with examples that leverage dependent typing.
3. **Edge Cases**: Test with nested types, types containing terms, and type-level functions.

## Minimal Change Scope

This plan follows a minimal change approach:
- Works within the existing reducer rather than creating new mechanisms
- Focuses on making the current system properly handle dependent types
- Maintains backward compatibility with existing code
- Improves correctness in a theoretically sound way 