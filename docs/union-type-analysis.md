# Union Type Handling Analysis

## Design Principles

Following our development practices:
1. Keep original terms in elaborated results
2. Only use reduction during type checking when necessary
3. Use proper reduction context from current context
4. Maintain clear documentation of changes

## Type Checking Architecture

### 1. Propagator-Based Type Checking
- Uses a constraint propagation network
- Each type constraint is represented by a propagator
- Cells hold type information and track their propagators
- Two types of propagator connections:
  - Reading: Propagators that read from a cell
  - Zonking: Propagators that can write to or resolve a cell

### 2. Union Type Representation
```scala
case class UnionOf(
    lhs: CellId[Term],
    rhs: Vector[CellId[Term]],
    cause: Expr
)
```
- Left-hand side (lhs) represents the union type
- Right-hand side (rhs) represents the component types
- Cause tracks the expression for error reporting

### 3. Type Unification Process
```scala
case class Unify(
    lhs: CellId[Term],
    rhs: CellId[Term],
    cause: Expr
)
```
- Handles type equality and subtyping
- Uses reduction only for type-level comparisons
- Special cases for union types:
  1. Union vs Union: Each RHS type needs LHS match
  2. Type vs Union: LHS must accept all RHS types
  3. Union vs Type: Any LHS type must accept RHS

## Current Test Cases

We have three key test cases that verify different aspects of union type handling:

1. **Widening (Success Case)** - `tests/tyck/union-subtype.chester`:
```chester
def f(x: Integer): Integer | String = x;
f(42);
```
This verifies that we can widen a type to a union type.

2. **Subtyping (Success Case)** - `tests/tyck/union-subtype.chester`:
```chester
def g(x: Integer | String): Integer | String = x;
let x: Integer = 42;
let y: Integer | String = g(x);
```
This verifies that subtyping works correctly with union types.

3. **Invalid Subtyping (Failure Case)** - `tests/tyck-fails/union-subtype-fail.chester`:
```chester
def f(x: Integer | String): Integer = x;
```
This verifies that we properly reject invalid subtyping.

## Implementation Analysis

### 1. Meta Variable Handling
Current implementation has several issues:
- Meta variables in unions need special propagation rules
- The `UnionOf` propagator needs to handle meta variables in both:
  ```scala
  case Some(Meta(lhsId)) =>
    // Need proper handling for meta variables in union LHS
  case (_, Some(Meta(rhsId))) =>
    // Need proper handling for meta variables in union RHS
  ```

### 2. Cell Coverage Issues
The propagator network has gaps:
- Some cells lack zonking propagators
- Union type components may not be properly connected
- Meta variables in unions may lose propagator connections

### 3. Type Reduction Strategy
Current implementation follows good practices:
- Only reduces during type equality checking
- Uses proper reduction context
- Preserves original terms in elaborated results

## Implementation Plan

### Phase 1: Coverage Verification
1. **Add Coverage Checks**
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

2. **Track Union Type Components**
   ```scala
   case class UnionTypeState(
     unionCell: CIdOf[Cell[?]],
     componentCells: Vector[CIdOf[Cell[?]]],
     metaVariables: Set[CIdOf[Cell[?]]]
   )
   ```

### Phase 2: Meta Variable Management
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

2. **Connection Verification**
   ```scala
   def verifyConnections(cell: CIdOf[Cell[?]]) = {
     // Verify reading propagators
     cell.readingPropagators.foreach(verifyPropagator)
     // Verify zonking propagators
     cell.zonkingPropagators.foreach(verifyPropagator)
   }
   ```

### Phase 3: Testing Strategy

1. **Coverage Tests**
   - Test cell coverage verification
   - Test union type component tracking
   - Test meta variable registration
   ```scala
   test("union type components have propagators") {
     // Test code
   }
   ```

2. **State Tests**
   - Test meta variable resolution
   - Test union type construction
   - Test subtyping rules
   ```scala
   test("meta variables in unions resolve correctly") {
     // Test code
   }
   ```

3. **Integration Tests**
   - Test complex union type scenarios
   - Test error reporting
   - Test performance characteristics
   ```scala
   test("complex union type hierarchy resolves") {
     // Test code
   }
   ```

## Next Steps

1. **Coverage Implementation**
   - [ ] Add propagator coverage verification
   - [ ] Add union type component tracking
   - [ ] Add connection verification

2. **Meta Variable Handling**
   - [ ] Implement meta variable tracking
   - [ ] Add meta variable resolution
   - [ ] Fix propagator connections

3. **Testing**
   - [ ] Add coverage test suite
   - [ ] Add state test suite
   - [ ] Add integration test suite

## Questions and Considerations

1. **Term Preservation**
   - How do we ensure original terms are preserved in all cases?
   - What is the impact on error reporting?

2. **Reduction Strategy**
   - When exactly do we need reduction for union types?
   - Are we following minimal reduction principle?

3. **Meta Variables**
   - How do we track meta variables in unions?
   - What constraints need to be propagated?

4. **Testing Strategy**
   - What additional test cases do we need?
   - How do we verify term preservation? 