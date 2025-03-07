# Dependent Type System Implementation Plan

This document details the step-by-step implementation plan for enhancing Chester's type checking system to better handle dependent types.

## Background

The current type checking system in Chester uses `NaiveReducer` with `ReduceMode.TypeLevel` for type equality checking. While this works for simple cases, it has limitations when dealing with dependent types where:

1. Types can depend on terms
2. Variable bindings in types require alpha-equivalence checking
3. Type equality requires more sophisticated reduction strategies

## Implementation Steps

### Step 1: Enhance Type Structure Reduction in NaiveReducer

**What**: Add a specialized helper method for reducing complex type structures.

**Why**: 
- Dependent types require more careful reduction of nested type structures
- Union and intersection types need special handling for their component types
- Type-level functions must be fully evaluated for proper comparison

**Implementation Details**:
```scala
// In NaiveReducer
private def reduceTypeStructure(term: Term)(using ctx: ReduceContext, r: Reducer): Term = {
  term match {
    // Handle complex type structures recursively
    case Union(types, meta) =>
      Union(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
      
    case Intersection(types, meta) =>
      Intersection(types.map(t => reduce(t, ReduceMode.TypeLevel)), meta)
    
    // Special handling for type-level functions
    case fcall: FCallTerm if isTypeFunction(fcall) =>
      // Fully reduce type functions
      val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)
      // Further reduce if still a complex type
      reduced match {
        case Union(_, _) | Intersection(_, _) => reduceTypeStructure(reduced)
        case _ => reduced
      }
      
    // Default
    case _ => term
  }
}
```

**Tests**:
- Run `sbt rootJVM/test` to verify basic functionality
- Ensure existing tests pass with the enhanced reducer

### Step 2: Add Alpha-Equivalence Checking in TyckPropagator

**What**: Implement proper alpha-equivalence checking for types in the `TyckPropagator`.

**Why**:
- In dependent type systems, variable bindings matter for type equality
- Current equality checking doesn't account for alpha-conversion
- Union and intersection types need set-based comparison

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
        // Check parameter types are equivalent
        val paramsEqual = params1.zip(params2).forall { case (p1, p2) =>
          // Use telescope type comparison for parameters
          telescopeTypesEqual(p1, p2)
        }
        
        if (!paramsEqual) false
        else {
          // For result type, need to check under equivalent variable bindings
          areAlphaEquivalent(result1, result2)
        }
      }
      
    // Set-based comparison for union/intersection types
    case (Union(types1, _), Union(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    case (Intersection(types1, _), Intersection(types2, _)) =>
      typesEquivalentModuloOrdering(types1, types2)
      
    // Other case handled by regular equality
    case _ => lhs == rhs
  }
}

// Helper for set-based comparison
private def typesEquivalentModuloOrdering(types1: Vector[Term], types2: Vector[Term])(using
    state: StateAbility[Tyck],
    localCtx: Context
): Boolean = {
  if (types1.length != types2.length) return false
  
  // Each type in each set must have an equivalent in the other set
  types1.forall(t1 => types2.exists(t2 => areAlphaEquivalent(t1, t2))) &&
  types2.forall(t2 => types1.exists(t1 => areAlphaEquivalent(t1, t2)))
}

// Helper for comparing telescope parameter types
private def telescopeTypesEqual(p1: TelescopeParam, p2: TelescopeParam): Boolean = {
  // Compare the types, names don't matter for alpha-equivalence
  areAlphaEquivalent(p1.ty, p2.ty)
}
```

**Tests**:
- Run `sbt rootJVM/test` to verify compilation and basic functionality
- Test with examples that use function types with dependencies

### Step 3: Integrate Alpha-Equivalence into Type Checking

**What**: Modify the `tryUnify` method to use alpha-equivalence checking.

**Why**:
- Current type checking uses direct equality after reduction
- Alpha-equivalence provides more accurate comparison for dependent types
- This completes the integration of our enhancements

**Implementation Details**:
```scala
def tryUnify(lhs: Term, rhs: Term)(using
    state: StateAbility[Tyck],
    localCtx: Context
): Boolean = {
  // ... existing code ...
  
  val lhsResolved = resolveReference(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
  val rhsResolved = resolveReference(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))

  if (lhsResolved == rhsResolved) true
  else {
    // Try alpha-equivalence before detailed matching
    if (areAlphaEquivalent(lhsResolved, rhsResolved)) true
    else {
      // Continue with existing case matching...
    }
  }
}
```

**Tests**:
- Run `sbt rootJVM/test` to verify all tests pass
- Especially check tests that involve function types and union/intersection types

### Step 4: Add Specialized Tests for Dependent Types

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

**Tests**:
- Create focused tests for dependent type functionality
- Run `sbt rootJVM/test` to verify all tests pass

## Verification Strategy

For each step:
1. Implement the changes as described
2. Run `sbt rootJVM/test` to verify compilation and tests
3. Fix any issues before proceeding to the next step
4. Commit changes with a descriptive message after each step

## Expected Results

After implementation:
1. The type checker will properly handle dependent types with improved reduction
2. Alpha-equivalence checking will provide more accurate type comparisons
3. All existing functionality will continue to work
4. The system will be better prepared for future dependent type extensions 