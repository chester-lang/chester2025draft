# Type Checking System: Propagator Network and Design

## Quick Start Guide

Chester's type checking system is powered by a **propagator network** - a constraint-based approach that allows for complex type relationships.

### Simple Visual Overview

```
                      ┌─────────────┐
                      │ Type System │
                      └─────────────┘
                             │
                             ▼
                     ┌───────────────┐
                     │ Type Checking │
                     └───────────────┘
                             │
                             ▼
┌────────────────────────────────────────────────────┐
│                 Propagator Network                 │
├────────────────┬─────────────────┬────────────────┤
│                │                 │                │
│    ┌───────┐   │    ┌───────┐    │   ┌───────┐   │
│    │ Cell  │◄──┼────┤Prop 1 │◄───┼───┤ Cell  │   │
│    └───────┘   │    └───────┘    │   └───────┘   │
│        ▲       │        ▲        │       ▲       │
│        │       │        │        │       │       │
│    ┌───────┐   │    ┌───────┐    │   ┌───────┐   │
│    │Prop 2 │◄──┼────┤ Cell  │◄───┼───┤Prop 3 │   │
│    └───────┘   │    └───────┘    │   └───────┘   │
│                │                 │                │
└────────────────┴─────────────────┴────────────────┘
```

### Key Concepts in 30 Seconds

1. **Cells** - Hold type information and track connections to propagators
2. **Propagators** - Define constraints between cells and propagate type information
3. **Network** - The collection of cells and propagators that work together

When type checking:
- Cells store type information (like "this variable has type Integer")
- Propagators enforce constraints (like "parameter type must match argument type")
- When a cell value changes, connected propagators activate to propagate that change

This reactive network allows complex type relationships (like union types) to be modeled effectively.

## Architecture

### 1. Core Components

1. **Cells (`HoldCell`)**
   - Hold type information and state
   - Track propagator connections:
     ```scala
     class HoldCell[+T <: CellRW[?,?]] {
       var store: CellRW[?,?]              // Current value
       var didChange: Boolean          // Change tracking
       var readingPropagators: Vector[PIdOf[Propagator[?]]]
       var zonkingPropagators: Vector[PIdOf[Propagator[?]]]
     }
     ```

2. **Propagators**
   - Base trait defining propagator behavior:
     ```scala
     trait Propagator[Ability] {
       def readingCells: Set[CellIdAny]
       def writingCells: Set[CellIdAny]
       def defaultingCells: Set[CellIdAny]
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
     for (cell <- propagator.defaultingCells) {
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

## Current Implementation Status

The implementation of the core type system features has been completed with recent significant enhancements. Major milestones include:

### 1. Cell Coverage Improvements (2025-04-21)

The previously "hacky" approach to cell coverage has been completely redesigned:
- Removed the `AutoConnect` propagator and related indirection
- Implemented direct type relationship handling during unification
- Created explicit relationships between types directly at creation/unification points
- Simplified codebase by removing several layers of indirection
- Maintained the same type checking capabilities with a cleaner implementation

### 2. Union Type Subtyping (2025-03-25)

Union types are now fully implemented with support for all subtyping relationships:
- **Union-to-Union**: `(A|B) <: (C|D)` with proper component compatibility
- **Specific-to-Union**: `A <: (B|C)` for cases like passing `Integer` to `Integer|String`
- **Union-to-Specific**: `(A|B) <: C` for returning unions from specific return type functions

### 3. Trait Implementation (2025-03-19)

Basic trait support is now available:
- Empty traits and record extension using `<:` syntax
- Trait-record subtyping relation in type system
- `TraitTypeTerm` representation with proper error reporting
- Context tracking for trait processing

### Remaining Challenges

1. **Type-Level Computation**
   - Further improvements to recursive type-level function applications
   - Advanced dependent types with multiple levels of abstraction
   - More comprehensive testing of type-level computation

2. **Advanced Trait Features**
   - Complete field requirement verification
   - Multiple trait inheritance
   - Trait methods and default implementations
   - Trait-to-trait inheritance constraints

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