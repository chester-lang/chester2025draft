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

2. **Specific-to-Union Subtyping**: `A <: (B|C)`
   - A specific type can be used where a union is expected if it's compatible with any union member
   - Example: Passing an `Integer` to a function expecting `Integer|String`

3. **Union-to-Specific Subtyping**: `(A|B) <: C`
   - A union can be assigned to a specific type if all union members are compatible with that type
   - Example: Returning an `Integer|Float` from a function that promises to return `Number`

#### Implementation Challenges

The union type subtyping implementation addresses several challenges:

1. **Cell Coverage**: Ensuring all cells (including component types) are properly covered by propagators
2. **Meta Variables in Unions**: Special handling for meta variables that appear in unions
3. **Early Return Prevention**: Avoiding early returns that could leave cells uncovered
4. **Component Tracking**: Ensuring each component of a union has proper propagator connections

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

#### 2. Comprehensive Coverage for Complex Types

For composite types like unions, it's essential to ensure coverage not only for the main type cell but also for all component cells.

#### 3. Avoiding Early Returns in Type Checking

Early returns in type checking code can sometimes leave cells uncovered. A safer approach is to ensure cells are covered before returning.

### 4. Type Level Unification Enhancement

Type unification is a critical operation in the type checking system. Recent improvements have enhanced how type levels are compared during unification:

```scala
// Enhanced type level comparison
case (Type(level1, _), Type(level2, _)) =>
  (level1, level2) match {
    case (LevelFinite(_, _), LevelUnrestricted(_)) => true // Finite is compatible with unrestricted
    case (LevelUnrestricted(_), LevelFinite(_, _)) => false // Unrestricted is not compatible with finite
    case _ => level1 == level2 // For other cases, keep the exact equality check
  }
```

This improvement enables:
1. **Flexibility with Unrestricted Levels**: A finite level type can now unify with an unrestricted level type
2. **Controlled Asymmetric Compatibility**: Unrestricted level types do not automatically unify with finite level types
3. **Unchanged Base Semantics**: For identical level types, the behavior remains the same

## Testing Strategy

### 1. Coverage Tests
- Test basic cell coverage
- Test union type component coverage
- Test meta variable coverage

### 2. State Tests
- Test propagator lifecycle
- Test union type state
- Test meta variable resolution

### 3. Integration Tests
- Test complex type scenarios
- Test error handling
- Test performance

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
2. **Trait-Record Compatibility**: Traits can be used where their implementing records are expected
3. **Trait-Trait Inheritance**: Traits can extend other traits

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
- **Handle already-filled cells gracefully**: When a cell already has a value, check if new value equals existing value.
- **Use debug assertions**: Include debug prints for cell operations to trace propagation flow.

#### 2. Propagator Implementation Guidelines

- **Declare cell dependencies correctly**: Ensure all propagators correctly declare their reading and zonking cell dependencies.
- **Implement naiveZonk correctly**: The `naiveZonk` method should return appropriate dependencies for zonking.
- **Be idempotent**: Propagator's `run` method should be idempotent - multiple calls with the same input should produce the same output.

#### 3. Type-Level Function Handling

When implementing propagators that handle type-level functions:

- **Watch for recursive effects**: Type-level functions may cause recursive propagation attempts
- **Avoid modifications during reduction**: When reducing for type equality, don't modify the original terms
- **Use correct reduction mode**: Use `ReduceMode.TypeLevel` for reductions needed for type equality checks