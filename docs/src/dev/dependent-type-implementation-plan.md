# Dependent Type System Implementation Plan

This document details the step-by-step implementation plan for enhancing Chester's type checking system to better handle dependent types.

## Background

The current type checking system in Chester uses `NaiveReducer` with `ReduceMode.TypeLevel` for type equality checking. While this works for simple cases, it has limitations when dealing with dependent types where:

1. Types can depend on terms
2. Variable bindings in types require alpha-equivalence checking
3. Type equality requires more sophisticated reduction strategies

## Implementation Status

### Completed Steps

#### 1. Enhanced Type Structure Reduction in NaiveReducer

**What was planned**: Add a specialized helper method for reducing complex type structures.

**What we found**: The codebase already had a `reduceTypeStructure` method with similar functionality!

The implemented method properly handles:
- Union types by recursively reducing their component types
- Intersection types with proper reduction
- Type-level function applications with recursive reduction for complex result types

**Implementation Details**:
```scala
// In NaiveReducer
private def reduceTypeStructure(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
  term match {
    // Handle complex type structures - recursively reduce their components
    case Union(types, meta) =>
      Union(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
      
    case Intersection(types, meta) =>
      Intersection(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
    
    // Type-level function applications should be fully reduced for consistency
    case fcall: FCallTerm if isTypeLevel(fcall) =>
      // First reduce normally
      val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)
      // Then check if the result needs further type structure handling
      reduced match {
        // If still a complex type after reduction, process it recursively
        case Union(_, _) | Intersection(_, _) => reduceTypeStructure(reduced)
        case _ => reduced
      }
      
    // Other terms are handled by standard reduction
    case _ => term
  }
}
```

#### 2. Alpha-Equivalence Checking in TyckPropagator

**What was planned**: Implement proper alpha-equivalence checking for types in the `TyckPropagator`.

**What we found**: The codebase already had `areAlphaEquivalent` and `typesEquivalentModuloOrdering` methods, but with a critical issue:
- The default case was returning `false` instead of falling back to regular equality

**What we fixed**:
- Updated the default case to use `lhs == rhs` instead of always returning `false`
- Fixed handling of TelescopeTerm objects in function types

**Implementation Details**:
```scala
// In TyckPropagator
private def areAlphaEquivalent(lhs: Term, rhs: Term)(using
    state: StateAbility[Tyck],
    localCtx: Context
): Boolean = {
  (lhs, rhs) match {
    // Function types with bound variables
    case (FunctionType(params1, result1, effects1, _), FunctionType(params2, result2, effects2, _)) =>
      if (params1.length != params2.length || effects1 != effects2) false
      else {
        val paramsEqual = params1.zip(params2).forall { case (p1, p2) =>
          // Check telescope parameters are equivalent
          if (p1.args.length != p2.args.length) false
          else {
            p1.args.zip(p2.args).forall { case (arg1, arg2) =>
              // For each argument, check that the types are alpha-equivalent
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
      
    // Set-based comparison for union/intersection types
    case (Union(types1, _), Union(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    case (Intersection(types1, _), Intersection(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    // For other cases, fall back to regular equality check
    case _ => lhs == rhs  // Changed from returning false
  }
}
```

#### 3. Integration of Alpha-Equivalence into Type Checking

**What was planned**: Modify the `tryUnify` method to use alpha-equivalence checking.

**What we found**: The codebase already had this integration in place! The `tryUnify` method was already checking for alpha-equivalence before proceeding to structural checks.

### Remaining Steps

#### 4. Add Specialized Tests for Dependent Types

**What**: Add specific tests that verify dependent type handling.

**Why**:
- Need to verify our changes work for actual dependent type scenarios
- Ensures we haven't broken existing functionality
- Provides regression protection for future changes

**Implementation Details**:
Create test cases that verify:
1. Function types with type dependencies
2. Equality of types with different variable names but same structure
3. Union and intersection types with alpha-equivalent components

## Verification Status

All tests pass with our implemented changes:
- Fixed implementation of `areAlphaEquivalent` method to fall back to equality for unhandled cases
- Fixed function type handling to properly check TelescopeTerm parameter types

## Next Steps

1. Add specialized tests for dependent types to verify the enhanced alpha-equivalence checking works correctly
2. Document the alpha-equivalence improvements in the codebase for future reference
3. Consider additional edge cases that might need handling 