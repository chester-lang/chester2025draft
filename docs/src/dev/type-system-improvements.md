# Chester Type System Improvements

This document outlines the current state, analysis, and planned improvements for Chester's type system, with a focus on union and intersection types.

## Current Architecture

### Propagator-Based Type Checking

Chester's type checker uses a constraint propagation network:

- Each type constraint is represented by a **propagator**
- **Cells** hold type information and track their propagators
- Two types of propagator connections:
  - **Reading**: Propagators that read from a cell
  - **Zonking**: Propagators that can write to or resolve a cell

### Type Representations

#### Union Types
```scala
case class UnionOf(
    lhs: CellId[Term],     // The union type
    rhs: Vector[CellId[Term]], // Component types
    cause: Expr            // For error reporting
)
```

#### Type Unification
```scala
case class Unify(
    lhs: CellId[Term],
    rhs: CellId[Term],
    cause: Expr
)
```

Special cases for union types:
1. Union vs Union: Each RHS type needs LHS match
2. Type vs Union: LHS must accept all RHS types
3. Union vs Type: Any LHS type must accept RHS

## Current Issues

### 1. Unimplemented Intersection Type Case

In `TyckPropagator.scala` (line 334), there's an unimplemented case for intersection types:
```scala
case (Intersection(_, _), Intersection(_, _)) => ???
```

While Chester doesn't yet support declaring intersection types in the source code, they are used internally (e.g., for integer literals).

### 2. Meta Variable Handling

Current implementation has issues with meta variables in unions:
```scala
case Some(Meta(lhsId)) =>
  // Need proper handling for meta variables in union LHS
case (_, Some(Meta(rhsId))) =>
  // Need proper handling for meta variables in union RHS
```

### 3. Cell Coverage Issues

The propagator network has gaps:
- Some cells lack zonking propagators
- Union type components may not be properly connected
- Meta variables in unions may lose propagator connections

## Planned Improvements

### 1. Intersection Type Implementation

```scala
case (Intersection(types1, _), Intersection(types2, _)) =>
  // For intersection types to be compatible, each type from the second intersection
  // must be compatible with at least one type from the first intersection
  types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))
```

**Benefits**:
- More robust type checking for internal intersection types
- Elimination of the unimplemented case
- Foundation for future explicit intersection type syntax

### 2. Meta Variable Management

1. **Meta Variable Tracking**
   ```scala
   def trackMetaVariable(id: CIdOf[Cell[?]]) = {
     case Some(Meta(metaId)) =>
       // Register meta variable
       metaVariables.add(metaId)
       // Ensure propagator connections
       verifyPropagatorCoverage(Vector(metaId))
   }
   ```

2. **Union Type Component Tracking**
   ```scala
   case class UnionTypeState(
     unionCell: CIdOf[Cell[?]],
     componentCells: Vector[CIdOf[Cell[?]]],
     metaVariables: Set[CIdOf[Cell[?]]]
   )
   ```

### 3. Cell Coverage Verification

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

## Testing Strategy

### 1. Intersection Type Testing

Since intersection types aren't directly expressible in Chester yet, we can test through integer literals:

```chester
// Test integer literals with different values
// (these use intersection types internally)
let a: Integer = 42;    // Positive integer
let b: Integer = -5;    // Negative integer
```

### 2. Union Type Testing Cases

We already have several key test cases:

1. **Widening (Success)**:
```chester
def f(x: Integer): Integer | String = x;
f(42);
```

2. **Subtyping (Success)**:
```chester
def g(x: Integer | String): Integer | String = x;
let x: Integer = 42;
let y: Integer | String = g(x);
```

3. **Invalid Subtyping (Failure)**:
```chester
def f(x: Integer | String): Integer = x;
```

### 3. Coverage and Integration Tests

1. **Cell Coverage Tests**
   ```scala
   test("union type components have propagators") {
     // Test code
   }
   ```

2. **Meta Variable Tests**
   ```scala
   test("meta variables in unions resolve correctly") {
     // Test code
   }
   ```

3. **Integration Tests**
   ```scala
   test("complex union type hierarchy resolves") {
     // Test code
   }
   ```

## Implementation Plan

### Phase 1: Core Type System Improvements
- [x] Document current type system architecture
- [ ] Implement intersection type unification case
- [ ] Add test cases for integer literals (internal intersection types)
- [ ] Verify implementation with existing tests

### Phase 2: Meta Variable and Cell Coverage
- [ ] Add propagator coverage verification
- [ ] Implement meta variable tracking
- [ ] Add union type component tracking
- [ ] Add connection verification tests

### Phase 3: Advanced Type Features
- [ ] Enhance union and intersection type interactions
- [ ] Implement explicit intersection type syntax (if needed)
- [ ] Add comprehensive test suite
- [ ] Update documentation

## Design Principles to Follow

1. **Term Preservation**
   - Keep original terms in elaborated results
   - Never reduce during elaboration
   - Only use reduction for type-level comparisons
   - Preserve source structure for better error reporting

2. **Reduction Strategy**
   - Only reduce during type equality checking
   - Use `ReduceMode.TypeLevel` for these internal reductions
   - Use proper reduction context from current context
   - Never reflect internal reductions in output

3. **Documentation**
   - Keep this document updated with implementation progress
   - Document design decisions and trade-offs
   - Maintain clear test cases for each feature 