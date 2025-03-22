# Type Checking System: Propagator Network and Design

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
     - Intersection types
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

### 3. Union Type Subtyping

Chester supports union types (`A|B`) with a sophisticated subtyping relationship managed by the propagator network. The subtyping rules are implemented in the `unify` method in `Elaborater.scala`.

#### Union Subtyping Rules

1. **Union-to-Union Subtyping**: `(A|B) <: (C|D)`
   - For each type in the right union, at least one type in the left union must accept it
   - Implemented by creating propagator connections between compatible component types
   - The `UnionOf` propagator ensures that cells are properly covered

2. **Specific-to-Union Subtyping**: `A <: (B|C)`
   - A specific type can be used where a union is expected if it's compatible with any union member
   - This is especially important for function parameters, where providing a more specific type should work
   - Example: Passing an `Integer` to a function expecting `Integer|String`

3. **Union-to-Specific Subtyping**: `(A|B) <: C`
   - A union can be assigned to a specific type if all union members are compatible with that type
   - This is critical for function returns, where returning a union type should work if all components are compatible
   - Example: Returning an `Integer|Float` from a function that promises to return `Number`

#### Implementation Details

The union subtyping implementation uses two key propagators:

1. **Unify Propagator**: Creates a direct connection between types
   ```scala
   state.addPropagator(Unify(toId(lhs), toId(rhs), cause))
   ```

2. **UnionOf Propagator**: Handles the relationship between a type and a collection of types
   ```scala
   state.addPropagator(UnionOf(targetType, unionComponentTypes, cause))
   ```

These propagators work together to ensure that all cells in the type graph are properly covered by at least one propagator, which is essential for the propagator network to function correctly.

#### Union-to-Union Subtyping Example

```scala
case (Union(types1, _), Union(types2, _)) if types1.nonEmpty && types2.nonEmpty => {
  // Ensure all cells are covered by propagators
  ensureCellCoverage(lhsCell, cause)
  ensureCellCoverage(rhsCell, cause)

  // Create direct connection between the union types
  state.addPropagator(Unify(lhsCell, rhsCell, cause))

  // Ensure all component types are covered
  val lhsTypeIds = types1.map(typ => {
    val cellId = toId(typ)
    ensureCellCoverage(cellId, cause)
    cellId
  }).toVector

  // Connect the unions to their components
  state.addPropagator(UnionOf(lhsCell, lhsTypeIds, cause))
  state.addPropagator(UnionOf(rhsCell, rhsTypeIds, cause))

  // Create connections between compatible component types
  for (t1 <- types1; t2 <- types2) {
    if (tryUnify(t1, t2)) {
      state.addPropagator(Unify(toId(t1), toId(t2), cause))
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

### 3. Cell Coverage Solutions

When cells lack proper coverage by propagators, type checking can fail during the zonking phase with errors like:

```
java.lang.IllegalStateException: Cells Vector(...) are not covered by any propagator
    at chester.utils.propagator.ProvideMutable$Impl.naiveZonk(ProvideMutable.scala:226)
    at chester.tyck.DefaultImpl.finalizeJudge(Elaborater.scala:557)
```

The following solutions have been implemented to address these issues:

#### 1. Self-Coverage Mechanism

Every cell should be covered by at least one propagator. A simple but effective approach is to ensure self-coverage:

```scala
// Helper method to ensure cell coverage
private def ensureCellCoverage(cell: CellId[Term], cause: Expr)(using
    state: StateAbility[Tyck],
    ctx: Context,
    ck: Tyck
): Unit = {
  // Create a self-referential propagator to ensure basic coverage
  state.addPropagator(UnionOf(cell, Vector(cell), cause))
}
```

This self-referential propagator creates a minimal but sufficient coverage for the cell, ensuring it won't be reported as uncovered during zonking.

#### 2. Comprehensive Coverage for Complex Types

For composite types like unions, it's essential to ensure coverage not only for the main type cell but also for all component cells:

```scala
// For union types, ensure coverage for all components
val unionTypeIds = unionTypes.map(typ => {
  val cellId = toId(typ)
  ensureCellCoverage(cellId, cause)
  cellId
}).toVector

// Then create the UnionOf propagator
state.addPropagator(UnionOf(unionCell, unionTypeIds, cause))
```

#### 3. Avoiding Early Returns in Type Checking

Early returns in type checking code can sometimes leave cells uncovered. A safer approach is to:

```scala
// Instead of returning early without ensuring coverage
if (lhsResolved == rhsResolved) {
  // Add necessary propagators first
  ensureCellCoverage(toId(lhsResolved), cause)
  ensureCellCoverage(toId(rhsResolved), cause)
  // Then return
  return
}
```

#### 4. Debugging Cell Coverage Issues

When troubleshooting cell coverage problems, targeted debug output helps identify which cells lack proper coverage:

```scala
if (DEBUG_CELL_COVERAGE) {
  println(s"=== CHECKING CELL COVERAGE ===")
  println(s"Cell: $cell")
  println(s"Reading propagators: ${state.readReadingPropagators(cell)}")
  println(s"Zonking propagators: ${state.readZonkingPropagators(cell)}")
}
```

These mechanisms ensure that all cells in the propagator network are properly covered, preventing errors during the zonking phase and enabling more complex type checking scenarios to work correctly.

### 4. Type Level Unification Enhancement

Type unification is a critical operation in the type checking system. Recent improvements have enhanced how type levels are compared during unification:

```scala
// Previous simple equality check
case (Type(level1, _), Type(level2, _)) =>
  level1 == level2

// Enhanced type level comparison
case (Type(level1, _), Type(level2, _)) =>
  (level1, level2) match {
    case (LevelFinite(_, _), LevelUnrestricted(_)) => true // Finite is compatible with unrestricted
    case (LevelUnrestricted(_), LevelFinite(_, _)) => false // Unrestricted is not compatible with finite
    case _ => level1 == level2 // For other cases, keep the exact equality check
  }
```

This improvement enables:
1. **Flexibility with Unrestricted Levels**: A finite level type can now unify with an unrestricted level type, increasing flexibility in the type system
2. **Controlled Asymmetric Compatibility**: Unrestricted level types do not automatically unify with finite level types, maintaining type safety
3. **Unchanged Base Semantics**: For identical level types, the behavior remains the same, preserving backward compatibility

This enhancement is particularly important for dependent types where the level of a type may need to vary based on its usage context.

### 5. Best Practices for Type Level Handling

When implementing type checking logic that deals with type level comparisons:

1. **Consider Level Compatibility Direction**: Remember that compatibility is directional - a finite level can flow into an unrestricted level, but not vice versa.

2. **Test with Different Level Combinations**: Test your code with various combinations of level types:
   ```scala
   // Test cases to consider
   val finite1 = LevelFinite(IntegerTerm(0, meta = None), meta = None)
   val finite2 = LevelFinite(IntegerTerm(1, meta = None), meta = None)  
   val unrestricted = LevelUnrestricted(meta = None)
   
   // These should succeed
   tryUnify(Type(finite1, meta = None), Type(unrestricted, meta = None))  // finite -> unrestricted
   tryUnify(Type(finite1, meta = None), Type(finite1, meta = None))       // same level
   
   // These should fail
   tryUnify(Type(unrestricted, meta = None), Type(finite1, meta = None))  // unrestricted -> finite
   ```

3. **Debug Level Mismatches**: When debugging type errors involving levels, check both the level types and their compatibility direction.

4. **Document Level Requirements**: In APIs that work with type levels, clearly document the level requirements.

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

## Trait Implementation

Chester's type system now supports traits and record-trait subtyping relationships through the `<:` syntax, with the following features implemented:

### 1. Trait Definition and Implementation

```scala
// Define a trait
trait WithName {
  def name: String;
}

// Record implementing a trait
record Person(name: String, age: Integer) <: WithName;

// Using the record with correct field
def getName(p: Person): String = p.name;
```

### 2. Trait Subtyping Rules

The type system implements several trait-related subtyping rules:

1. **Record-Trait Subtyping**: Records that extend traits are considered subtypes of those traits
   ```scala
   case (RecordTypeTerm(recordDef, _, _), TraitTypeTerm(traitDef, _)) =>
     // Check if the record implements the trait
     checkTraitImplementation(recordDef, traitDef, cause)
   ```

2. **Trait-Record Compatibility**: Traits can be used where their implementing records are expected
   ```scala
   case (TraitTypeTerm(traitDef, _), RecordTypeTerm(recordDef, _, _)) =>
     // Ensure the record implements the trait
     checkTraitImplementation(recordDef, traitDef, cause)
   ```

3. **Trait-Trait Inheritance**: Traits can extend other traits
   ```scala
   case (TraitTypeTerm(childTraitDef, _), TraitTypeTerm(parentTraitDef, _)) =>
     // Check if child trait extends parent trait
     checkTraitExtends(childTraitDef, parentTraitDef, cause)
   ```

### 3. Implementation Components

The trait implementation consists of several key components:

1. **TraitTypeTerm** in `Term.scala` for trait type representation
2. **TraitStmtTerm** for trait definitions with optional bodies
3. **processTraitStmt** in `ElaboraterBlock.scala` to handle trait declarations
4. **checkTraitImplementation** in `TyckPropagator.scala` to verify trait implementation
5. Special context tracking with `withProcessingType` to handle trait bodies

### 4. Future Enhancements

Future work will focus on:
- Complete field requirement verification
- Multiple trait inheritance
- Trait methods and default implementations

## Best Practices for Cell Management

### OnceCell Usage Guidelines

The `OnceCell` implementation is a specialized cell type that enforces single-assignment semantics. Proper usage requires careful attention to avoid errors:

#### 1. Cell Filling Best Practices

- **Always check before filling**: Before calling `fill()` on any cell, check if it already has a value using `state.readUnstable(cell)`.
  ```scala
  val existingValue = state.readUnstable(cell)
  if (existingValue.isEmpty) {
    state.fill(cell, value)
  } else {
    // Handle already filled case
  }
  ```

- **Handle already-filled cells gracefully**: When a cell already has a value:
  - If the new value equals the existing value, skip the fill operation
  - If different, log appropriate diagnostics
  ```scala
  if (existingValue.get == newValue) {
    // Same value, no action needed
  } else {
    // Log warning or handle difference
  }
  ```

- **Use debug assertions**: Include debug prints for cell operations to trace propagation flow
  ```scala
  Debug.debugPrint(DebugCategory.Cell, s"Attempting to fill cell: $cell")
  Debug.debugPrint(DebugCategory.Cell, s"Current value: $existingValue")
  Debug.debugPrint(DebugCategory.Cell, s"New value: $newValue")
  ```

#### 2. Propagator Implementation Guidelines

- **Declare cell dependencies correctly**: Ensure all propagators correctly declare their reading and zonking cell dependencies
  ```scala
  override val readingCells: Set[CellIdAny] = Set(inputCell1, inputCell2)
  override val writingCells: Set[CellIdAny] = Set(outputCell)
  override val zonkingCells: Set[CellIdAny] = Set(outputCell)
  ```

- **Implement naiveZonk correctly**: The `naiveZonk` method should return appropriate dependencies for zonking
  ```scala
  override def naiveZonk(needed: Vector[CellIdAny]): ZonkResult = {
    if (needed.contains(outputCell)) {
      ZonkResult.Require(Vector(inputCell1, inputCell2))
    } else {
      ZonkResult.NotNeeded
    }
  }
  ```

- **Be idempotent**: Propagator's `run` method should be idempotent - multiple calls with the same input should produce the same output

#### 3. Type-Level Function Handling

When implementing propagators that handle type-level functions:

- **Watch for recursive effects**: Type-level functions may cause recursive propagation which can attempt to fill the same cell multiple times
- **Avoid modifications during reduction**: When reducing for type equality, don't modify the original terms
- **Use correct reduction mode**: Use `ReduceMode.TypeLevel` for reductions needed for type equality checks
- **Check results of unification**: After unifying types, verify the cell state before proceeding

#### 4. Testing Strategies for Cell Management

- **Create test cases that involve multiple fills**: Test cell behavior with patterns that could lead to multiple fill attempts
- **Test type-level function behaviors**: Include test cases that exercise type-level function applications
- **Enable debug logging**: Use `Debug.enable(DebugCategory.Cell)` to trace cell operations during tests
- **Test edge cases**: Specifically test cases where the same function might be evaluated multiple times

By following these guidelines, propagators can safely manage cells without triggering exceptions or creating inconsistent states.