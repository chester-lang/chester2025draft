# Chester Elaboration System

## Introduction

Chester features two elaboration systems for type checking:

1. **Original Type Checking Logic (`chester.tyck`)**: The constraint-based type checking system using propagator networks
2. **New Elaboration System (`chester.elab`)**: A modernized, more modular approach to elaboration with a dedicated solver

This document explains both systems, their differences, and provides guidance on how to work with the new elaboration system.

## Architectural Comparison

### Original System (`chester.tyck`)

The original type checking system is built around propagator networks with cells and constraints:

- Uses `TyckPropagator` for constraint propagation
- Has a monolithic `Elaborater` class with specialized components (`ElaboraterBlock`, `ElaboraterFunction`, etc.)
- Relies on `CellId` references for tracking types
- Uses a stateful approach for tracking and resolving constraints

### New System (`chester.elab`)

The new elaboration system takes a fundamentally different approach to type checking:

- **Constraint-based Solver**: Uses a dedicated solver architecture for tracking and resolving typing constraints
- **Modular Handler System**: Each elaboration concern is handled by a dedicated, composable handler
- **Cell-based Representation**: Uses cells to represent type variables and constraints in a more structured way
- **More Precise Types**: Infers more specific types (e.g., `IntTerm` instead of `IntegerTerm`)
- **Handler-driven Architecture**: Components like `BlockElabHandler` and `ListOfHandler` encapsulate specific elaboration logic

### Key Components of the New System

#### 1. Core Interfaces

**Elab Trait** (`Elab.scala`) serves as the primary interface for elaboration operations. It provides three key methods:

- `elab`: Elaborates an expression against a given type, returning a cell containing the elaborated term
- `infer`: Infers both the term and type for an expression, returning them as a pair
- `inferType`: Specializes in type-checking expressions that should be types themselves

All these methods require appropriate context, effects tracking, and solver operations to function.

**DefaultElab Implementation** provides the core elaboration logic for different expression types. It uses pattern matching to handle different kinds of expressions, dispatching each to an appropriate constraint handler:

- Integer literals are handled by the IntegerLit constraint
- String literals are handled by the StringLit constraint
- List expressions are handled by the ListOf constraint
- Blocks are handled by the BlockElab constraint

For each expression type, a corresponding constraint is created and passed to the solver through SolverOps.

#### 2. Entry Point

**DefaultElaborator** (`Elaborator.scala`) is the main entry point for using the new elaboration system. It's configured with:

- A default Elab implementation (DefaultElabImpl)
- A SolverFactory (ConcurrentSolverFactory)
- A handler configuration (DefaultSolverConf)

This setup provides the `inferPure()` method that is used by the REPL and tests as the primary entry point for type checking expressions.

#### 3. Constraint Handlers

The system uses a handler-based architecture where each type of constraint has a dedicated handler:

- **Literal Handlers**: `IntegerLitHandler`, `StringLitHandler`, `SymbolLitHandler`
- **Block Handler**: `BlockElabHandler` for elaborating code blocks
- **List Handler**: `ListOfHandler` for list expressions
- **Unification Handlers**: `UnifyHandler`, `UnifyMultipleHandler` for type compatibility
- **Type Handlers**: `IsTypeHandler` for type checking, `SimplifyUnionHandler` for union types

Each handler implements the `Handler` trait with a `run` method that processes a specific type of constraint.

#### 4. Operations Interface

**ElabOps** (`ElabOps.scala`) provides operations for error reporting and semantic collection:

```scala
case class ElabOps(reporter: Reporter[TyckProblem], collector: SemanticCollector) extends Reporter[TyckProblem] {
  // Delegated reporter methods
  override def report(problem: TyckProblem): Unit = reporter.report(problem)
}
```

## Current Implementation Status

### Features Supported

The new elaboration system currently supports:

- Basic literals (integers, strings, symbols)
- Lists (including heterogeneous and nested lists with correct union typing)
- Code blocks with statements and expressions
- Type unification and compatibility checking
- Pure expressions (without effects)

### REPL Integration

The REPL engine now uses the new elaboration system by default, as seen in `REPLEngine.scala`:

The REPL implementation includes a toggle (`useNewElab`) set to true by default that allows switching between the old and new elaboration systems. When enabled, the typeCheck method creates a reporter and ElabOps, then calls DefaultElaborator.inferPure() to type check expressions. The results are wrapped in a TyckResult0 object to maintain compatibility with the old system's result format.

This implementation allows seamless switching between the old and new elaboration systems, with the new system as the default.

### Test Coverage

Test coverage for the new system is implemented in `ElabLiteralAndListTest.scala`, which verifies:

- **Integer literals**: Correctly elaborated to `IntTerm` with `IntType`
- **Heterogeneous lists**: Elaborated to `ListTerm` with a union type for elements
- **Empty lists**: Properly typed as `ListTerm[NothingType]`
- **Nested lists**: Correctly handle nested list structures and their type relationships

These tests demonstrate the system's ability to:  
1. Infer precise types (using `IntTerm` instead of the more general `IntegerTerm`)
2. Handle heterogeneity through proper union type creation
3. Maintain correct type relationships in nested structures

## Using the New Elaboration System

### Basic Usage

The following example shows how to use the new elaboration system to type check an expression:

To use the new elaboration system, you'll need to:

1. Parse an expression using ChesterReaderV2 or another parser
2. Create a reporter and ElabOps for error collection
3. Call DefaultElaborator.inferPure() to obtain a Judge containing the elaborated term and type
4. Check for errors and access the elaborated term and inferred type

This process will properly handle parsing and type checking of various expressions, including heterogeneous lists that will be typed with appropriate union types.

### Extending the System with New Expression Types

To add support for a new expression type, you need to follow these steps:

#### 1. Define a Constraint Kind

Create a Kind object for your constraint:

#### 1. Define a Constraint Kind

Create a Kind object in the `chester.elab` package that defines the type of your constraint. This serves as a unique identifier for your constraint type in the system.

#### 2. Create a Constraint Class

Define a constraint class for your expression type that takes:
- Your expression type as a parameter
- The target type cell
- Required implicit parameters (effects, elab, ops, ctx)

The constraint class should extend the `Constraint` abstract class with your Kind.

#### 3. Implement a Handler

Create a handler that processes your constraint by implementing the `Handler` trait. The handler needs to:
- Override the `run` method to implement the elaboration logic
- Create appropriate cells for your results
- Connect your result to the target type using constraints like `Unify`
- Optionally implement defaulting behavior for when type information is incomplete

#### 4. Register the Handler

Add your handler to `DefaultSolverConf.scala` so the system knows how to process your constraint. This involves adding your handler to the list of handlers in the `DefaultSolverConf` value.

#### 5. Update DefaultElab

Finally, extend the `elab()` method in `DefaultElab` to handle your expression type by adding a pattern matching case for your expression type that calls your constraint.

### Example: Adding Support for Boolean Literals

A practical example would be adding support for boolean literals, which would require:

1. Defining a `BooleanLit` Kind to identify the boolean literal constraint
2. Creating a `BooleanLit` constraint class that takes a BooleanLiteral expression and target type
3. Implementing a `BooleanLitHandler` that:
   - Creates a BooleanTerm with the appropriate value
   - Adds a cell containing that term
   - Creates a BooleanType cell
   - Unifies the target type with the boolean type
   - Connects the boolean term to the output cell
4. Registering the handler in DefaultSolverConf
5. Adding a case for BooleanLiteral expressions in the DefaultElab.elab method

## Transition Guidelines

While both systems currently coexist, the development focus is transitioning to the new `chester.elab` system. Follow these guidelines when working with the codebase:

### For New Development

- Use `DefaultElaborator.inferPure()` as the primary entry point for new typechecking code
- Implement new features and extensions in the `chester.elab` package
- Add handlers for new expression types following the pattern shown above
- Write tests specifically against the new elaboration system

### For Maintenance of Existing Code

- When fixing bugs in the existing `chester.tyck` system, consider if the fix should also be applied to `chester.elab`
- Document cross-references between equivalent functionality in both systems
- Gradually migrate test cases to support the new system

### Testing Approach

- Use `ElabLiteralAndListTest.scala` as a reference for test structure and pattern
- Create test cases that work with both systems to ensure compatibility
- The REPL's toggle (`useNewElab`) allows easy switching between systems for comparison

## Best Practices

### 1. Preserve Original Terms

Consistent with the existing guidelines for the original elaboration system:

- The elaboration system should preserve the original structure of terms
- Avoid reduction during elaboration
- Keep source code structure intact in elaborated results
- Only use reduction internally during type checking when absolutely necessary

### 2. Error Reporting

- Use the `ElabOps` reporter for consistent error messages
- Provide detailed type information in error messages
- Match the error format of the original system for consistency
- Include source position information when available
- Use internationalized messages (with `t""` string templates)

### 3. Testing

- Test both success and failure cases
- Verify the structure of elaborated terms
- Check inferred types carefully, especially for complex cases like union types
- Test with heterogeneous data to verify union type handling
- Ensure tests cover edge cases like empty collections and nested structures

## Current Limitations and Future Work

The new elaboration system is still under development and doesn't yet support the full range of Chester language features. Current limitations include:

- Limited support for complex expressions and statements
- Incomplete handling of advanced type features like traits and interfaces
- Partial support for effects system
- Incomplete support for pattern matching

Future development should focus on:

1. Extending the system to support all Chester language features
2. Improving error messages and diagnostics
3. Enhancing performance and error recovery
4. Eventually replacing the original system entirely
