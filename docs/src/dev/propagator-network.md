# Propagator Network Implementation

## Overview

The propagator network is a key component of Chester's type checking system. It manages the flow of type information and constraints between different parts of the program.

## Architecture

### 1. Core Components

1. **Cells (`HoldCell`)**
   - Hold type information and state
   - Track propagator connections:
     ```scala
     class HoldCell[+T <: Cell[?]] {
       var store: Cell[?]              // Current value
       var didChange: Boolean          // Change tracking
       var readingPropagators: Vector[PIdOf[Propagator[?]]]
       var zonkingPropagators: Vector[PIdOf[Propagator[?]]]
     }
     ```

2. **Propagators**
   - Base trait defining propagator behavior:
     ```scala
     trait Propagator[Ability] {
       def readingCells: Set[CIdOf[Cell[?]]]
       def writingCells: Set[CIdOf[Cell[?]]]
       def zonkingCells: Set[CIdOf[Cell[?]]]
       def run(using state: StateAbility[Ability], more: Ability): Boolean
       def naiveZonk(needed: Vector[CellIdAny]): ZonkResult
     }
     ```

### 2. Key Propagator Types

1. **Unify Propagator**
   - Handles type unification and subtyping
   - Special cases for:
     - Meta variables
     - Union types
     - List types
     - Record types

2. **UnionOf Propagator**
   - Manages union type construction
   - Handles:
     - Component type collection
     - Meta variable resolution
     - Type compatibility checks

3. **LiteralType Propagator**
   - Handles literal type inference
   - Manages type constraints for literals

### 3. Propagation Process

1. **Registration**
   ```scala
   def addPropagatorGetPid[T <: Propagator[Ability]](propagator: T) {
     // 1. Create propagator holder
     val id = new HoldPropagator[T](uniqId, propagator)
     
     // 2. Register with reading cells
     for (cell <- propagator.readingCells) {
       cell.readingPropagators :+= id
     }
     
     // 3. Register with zonking cells
     for (cell <- propagator.zonkingCells) {
       cell.zonkingPropagators :+= id
     }
     
     // 4. Initial run
     if (propagator.run) {
       id.alive = false
     }
   }
   ```

2. **Execution**
   ```scala
   def tick(using more: Ability): Unit = {
     while (didChanged.nonEmpty) {
       val id = didChanged.removeHead()
       if (id.didChange) {
         id.didChange = false
         // Run reading propagators
         for (p <- id.readingPropagators if p.alive) {
           if (p.store.run) {
             p.alive = false
           }
         }
       }
     }
   }
   ```

## Current Implementation Issues

### 1. Cell Coverage Gaps

1. **Union Type Components**
   ```scala
   case class UnionOf(...) {
     override val zonkingCells = Set(lhs) ++ rhs.toSet
     // Issue: Components may not get proper propagators
   }
   ```

2. **Meta Variables in Unions**
   ```scala
   // Current issues:
   case (Some(Meta(id)), _) =>
     // 1. No tracking of meta variables
     // 2. No verification of propagator connections
     // 3. Potential loss of constraints
   
   // Need to handle:
   case Some(Meta(lhsId)) =>
     // Track meta variable
     metaVariables.add(lhsId)
     // Ensure propagator connections
     verifyPropagatorCoverage(Vector(lhsId))
   ```

### 2. Propagator Lifecycle Issues

1. **Cleanup and State Management**
   ```scala
   class PropagatorState(
     val id: PIdOf[Propagator[?]],
     var alive: Boolean,
     var lastRun: Long,
     var connectedCells: Set[CIdOf[Cell[?]]],
     var metaVariables: Set[CIdOf[Cell[?]]]
   ) {
     def markDone(): Unit = {
       // Verify all constraints satisfied
       require(verifyConstraints(), "Constraints not satisfied")
       // Verify all cells resolved
       require(verifyResolution(), "Cells not resolved")
       // Then mark as done
       alive = false
     }
   }
   ```

2. **Union Type State Tracking**
   ```scala
   class UnionTypeState(
     val unionCell: CIdOf[Cell[?]],
     val components: Vector[CIdOf[Cell[?]]],
     val metaVars: Set[CIdOf[Cell[?]]]
   ) {
     def verifyState(): Boolean = {
       // Verify union cell has propagators
       verifyPropagatorCoverage(Vector(unionCell)) &&
       // Verify components have propagators
       components.forall(verifyPropagatorCoverage) &&
       // Verify meta vars have propagators
       metaVars.forall(verifyPropagatorCoverage)
     }
   }
   ```

## Implementation Plan

### Phase 1: Coverage Verification

1. **Add Coverage Checks**
   ```scala
   trait CoverageVerification {
     // Verify cell has required propagators
     def verifyCell(cell: CIdOf[Cell[?]]): Boolean
     
     // Verify propagator connections valid
     def verifyPropagator(prop: PIdOf[Propagator[?]]): Boolean
     
     // Verify union type state
     def verifyUnionType(union: UnionTypeState): Boolean
   }
   ```

2. **Track Type States**
   ```scala
   class TypeStateTracker {
     // Track union types
     val unionTypes = mutable.Map[CIdOf[Cell[?]], UnionTypeState]()
     
     // Track meta variables
     val metaVars = mutable.Set[CIdOf[Cell[?]]]()
     
     // Track propagator states
     val propagators = mutable.Map[PIdOf[Propagator[?]], PropagatorState]()
   }
   ```

### Phase 2: Connection Management

1. **Safe Registration with Meta Variables**
   ```scala
   def safeRegisterPropagator[T <: Propagator[?]](
     propagator: T,
     cells: Set[CIdOf[Cell[?]]],
     metaVars: Set[CIdOf[Cell[?]]] = Set.empty
   ) = {
     // Verify cells
     cells.foreach(verifyCell)
     // Track meta variables
     metaVars.foreach(trackMetaVar)
     // Register propagator
     val pid = addPropagatorGetPid(propagator)
     // Update state
     updatePropagatorState(pid, cells, metaVars)
   }
   ```

2. **Connection Verification with Union Types**
   ```scala
   def verifyUnionConnections(
     union: UnionTypeState
   ): Boolean = {
     // Verify union cell connections
     verifyConnections(union.unionCell) &&
     // Verify component connections
     union.components.forall(verifyConnections) &&
     // Verify meta variable connections
     union.metaVars.forall(verifyConnections)
   }
   ```

## Testing Strategy

### 1. Coverage Tests
- Test basic cell coverage
- Test union type component coverage
- Test meta variable coverage
```scala
class CoverageTests {
  test("cells have required propagators") {
    // Test basic cell coverage
  }
  
  test("union types fully connected") {
    // Test union type coverage
  }
  
  test("meta variables properly tracked") {
    // Test meta variable coverage
  }
}
```

### 2. State Tests
- Test propagator lifecycle
- Test union type state
- Test meta variable resolution
```scala
class StateTests {
  test("propagator lifecycle correct") {
    // Test lifecycle
  }
  
  test("union type state maintained") {
    // Test union state
  }
  
  test("meta variables resolve") {
    // Test resolution
  }
}
```

### 3. Integration Tests
- Test complex type scenarios
- Test error handling
- Test performance
```scala
class IntegrationTests {
  test("complex type hierarchies") {
    // Test complex scenarios
  }
  
  test("error handling correct") {
    // Test error cases
  }
  
  test("performance acceptable") {
    // Test performance
  }
}
```

## Next Steps

1. **Implementation**
   - [ ] Add type state tracking
   - [ ] Add coverage verification
   - [ ] Add connection management

2. **Testing**
   - [ ] Implement coverage tests
   - [ ] Implement state tests
   - [ ] Implement integration tests

3. **Documentation**
   - [ ] Document type state rules
   - [ ] Document verification process
   - [ ] Document testing approach 