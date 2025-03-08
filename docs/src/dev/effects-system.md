# Algebraic Effects System for Chester

## Overview

This document outlines the implementation of an algebraic effects system for the Chester language. The initial implementation will focus on basic checking rules without handling higher-order functions, which will be addressed in future work.

## What are Algebraic Effects?

Algebraic effects are a way to model computational effects in a compositional manner. They provide several advantages over traditional approaches:

1. **Separation of Concerns**: Effect declaration is separate from effect handling
2. **Composability**: Effects can be freely combined
3. **Type Safety**: Effects are tracked in the type system
4. **Local Reasoning**: Effect handling is lexically scoped
5. **Reusability**: Effect handlers can be reused for different computations

Unlike exceptions or monads, algebraic effects allow for:
- Resumable computations after handling an effect
- Multiple effect handlers in different parts of the program
- Clear type-level tracking of possible effects

## Current Status

Chester already has a rudimentary effect system with the following components:

1. **Effects Representation**: Effects are represented as a map of `LocalV` to `Term` in the `Effects` class
2. **Effects Cell**: Basic infrastructure exists for tracking effects during type checking via `EffectsCell`, `DynamicEffectsCell`, and `FixedEffectsCell`
3. **Effects in Function Types**: Functions can declare effects in their type signatures

However, the system is incomplete and lacks:
- Proper effect checking rules
- Effect operations (performing effects)
- Effect handlers
- Effect inference
- Effect subtyping

## Design Goals

The algebraic effects system for Chester aims to:

1. **Be Explicit**: Effects should be declared in type signatures
2. **Be Composable**: Multiple effects should be easily combined
3. **Be Type-Safe**: The type system should track and verify effects
4. **Be Efficient**: Effect checking should not significantly impact compile times
5. **Be Intuitive**: The syntax and semantics should be easy to understand

## Implementation Plan

Following the guidelines in `development.md`, we will implement the effects system in a careful, step-by-step manner:

### Phase 1: Core Effect Checking

1. **Document Before Implementing**
   - This document serves as the initial planning step
   - Each implementation step will be documented before coding
   - Changes will be broken down into small, testable units

2. **Enhance Effect Representation**
   - Update the `Effects` class to better represent effect sets
   - Implement effect operation resolution
   - Add effect checking in function bodies

3. **Implement Basic Effect Checking Rules**
   - Effect propagation for function calls
   - Effect checking for expressions
   - Pure function verification

4. **Effect Type Errors**
   - Define error types for effect checking failures
   - Implement helpful error messages

5. **Testing Strategy**
   - Create unit tests for each effect checking rule
   - Verify error reporting works correctly
   - Test combinations of effects
   - Run the full test suite after each change

### Phase 2: Effect Handlers (Future Work)

1. **Handler Representation**
   - Define syntax for effect handlers
   - Represent handlers in the type system

2. **Handler Checking Rules**
   - Verify handlers implement all operations
   - Check effect elimination in handlers

3. **Resumable Operations**
   - Implement the ability to resume after handling an effect

### Phase 3: Higher-Order Functions (Future Work)

1. **Effect Polymorphism**
   - Allow functions to be parameterized by effects
   - Check higher-order function application

2. **Effect Inference**
   - Infer effects for unannotated functions
   - Implement bidirectional effect checking

## Effect System Design

### 1. Effect Declaration

Effects will be declared as part of function types. For the initial implementation, we'll use a simple syntax:

```chester
// Function with an IO effect
def readLine(): String / IO = {
  // Implementation
}

// Function with multiple effects
def processInput(): Result / {IO, Error} = {
  // Implementation
}

// Function with no effects (pure function)
def add(a: Int, b: Int): Int = {
  a + b
}
```

### 2. Effect Definition

Effects will be defined as interfaces with operation signatures:

```chester
effect IO {
  def readLine(): String
  def println(s: String): Unit
}

effect Error {
  def raise(message: String): Nothing
}

effect State[T] {
  def get(): T
  def set(value: T): Unit
}
```

### 3. Effect Checking Rules

The initial implementation will focus on the following effect checking rules:

1. **Effect Propagation**: Functions that call effectful functions must declare those effects
   ```chester
   def readAndProcess(): Result / IO = {
     val input = readLine()  // readLine has IO effect
     process(input)          // process is pure
   }
   ```

2. **Effect Subsumption**: A function can declare more effects than it actually uses
   ```chester
   def maybeRead(condition: Boolean): String / IO = {
     if (condition) {
       readLine()  // Uses IO effect
     } else {
       "default"   // No effect used, but function still declares IO
     }
   }
   ```

3. **Effect Checking in Function Bodies**: Body expressions must respect the function's effect declaration
   ```chester
   // Error: function body uses IO effect but declaration doesn't include it
   def silentRead(): String = {
     readLine()  // Error: IO effect not declared
   }
   ```

4. **Pure Function Checking**: Functions with no declared effects cannot use effectful operations
   ```chester
   def add(a: Int, b: Int): Int = {
     println("Adding")  // Error: IO effect not declared
     a + b
   }
   ```

### 4. Effect Operations (Performing Effects)

Effects will be performed by calling the operations defined in the effect interface:

```chester
def greet(): Unit / IO = {
  val name = readLine()  // Performs the IO.readLine effect
  println("Hello, " + name)  // Performs the IO.println effect
}
```

## Detailed Checking Rules

### Effect Checking Judgment

We define the judgment `Γ ⊢ e : τ / ε` to mean "in context Γ, expression e has type τ and effects ε".

#### Basic Rules

1. **Variables and Constants (No Effects)**
   ```
   Γ ⊢ x : τ / {}   (where x is a variable or constant)
   ```

2. **Function Definition**
   ```
   Γ, x:τ₁ ⊢ e : τ₂ / ε
   ------------------------------
   Γ ⊢ λx:τ₁.e : τ₁ → τ₂ / ε / {}
   ```

3. **Function Application**
   ```
   Γ ⊢ e₁ : τ₁ → τ₂ / ε₁ / ε₂
   Γ ⊢ e₂ : τ₁ / ε₃
   --------------------------------
   Γ ⊢ e₁(e₂) : τ₂ / (ε₁ ∪ ε₂ ∪ ε₃)
   ```

4. **Effect Subsumption**
   ```
   Γ ⊢ e : τ / ε₁
   ε₁ ⊆ ε₂
   -------------------
   Γ ⊢ e : τ / ε₂
   ```

5. **Let Binding**
   ```
   Γ ⊢ e₁ : τ₁ / ε₁
   Γ, x:τ₁ ⊢ e₂ : τ₂ / ε₂
   ---------------------------
   Γ ⊢ let x = e₁ in e₂ : τ₂ / (ε₁ ∪ ε₂)
   ```

6. **Performing Effects**
   ```
   eff is an effect type with operation op : τ₁ → τ₂
   Γ ⊢ e : τ₁ / ε
   --------------------------------------
   Γ ⊢ perform eff.op(e) : τ₂ / (ε ∪ {eff})
   ```

## Technical Implementation Details

### Integration with Existing Type Checker

The effect checking will be integrated with the existing type checker by:

1. **Enhancing the Current Effects Representation**
   ```scala
   // Current implementation in simple.scala
   case class Effects(effectss: Map[LocalV, Term] = HashMap.empty, meta: OptionTermMeta) 
     extends EffectsM with WHNF with EffectsC[Term]
   
   // We'll enhance this to better represent effect sets and operations
   ```

2. **Updating ElaboraterCommon**
   ```scala
   // Current implementation has:
   trait EffectsCell extends Cell[Effects] {
     def requireEffect(effect: Term)(using Tyck, StateAbility[Tyck]): LocalV = {
       ???
     }
   }
   
   // We'll implement the requireEffect method and add additional effect checking
   ```

3. **Modifying Function Type Checking**
   - Update function type checking to verify effect declarations
   - Implement effect propagation during function calls
   - Add error reporting for effect violations

4. **Adding Effect-Specific Error Types**
   - Create new error types for effect-related issues
   - Implement helpful error messages that guide users

### Effect Checking Algorithm

The effect checking algorithm will traverse expressions and:

1. For function calls, collect effects from the function type and arguments
2. For function definitions, check that the body's effects are covered by the declaration
3. For effect operations, add the effect to the current effect set
4. For let bindings, combine effects from the initializer and body

## Testing Strategy

Following the testing requirements in `development.md`:

1. **Unit Tests for Effect Checking Rules**
   - Test each core rule individually
   - Verify correct error reporting for violations
   - Test both success and failure cases

2. **Integration Tests**
   - Test combinations of effects
   - Verify effect propagation across function boundaries
   - Run the full test suite (`sbt rootJVM/test`) after each change

3. **Examples and Use Cases**
   - Implement common effect patterns as test cases
   - Test realistic scenarios combining multiple effects

## Verification Process

For each implementation step:

1. Document the planned changes
2. Implement one logical change at a time
3. Run the full test suite
4. Review changes with `git diff | cat`
5. Commit with a clear message
6. Verify the commit with `git diff HEAD^ HEAD | cat`

## Future Directions

1. **Effect Polymorphism**: Allow functions to be parameterized by effect variables
2. **Effect Subtyping**: Define a subtyping relation between effect sets
3. **Effect Inference**: Infer effects for unannotated functions
4. **Advanced Effect Handlers**: Support for multi-effect handlers and effect transformation
5. **Optimization**: Implement effect-based optimizations 

## References

1. Plotkin, G., & Pretnar, M. (2009). Handlers of algebraic effects. In Programming Languages and Systems.
2. Leijen, D. (2017). Type directed compilation of row-typed algebraic effects. In Proceedings of POPL.
3. Brachthäuser, J. I., & Schuster, P. (2017). Effekt: Extensible algebraic effects in Scala.
4. Lindley, S., McBride, C., & McLaughlin, C. (2017). Do be do be do. In Proceedings of POPL.
5. Zhang, Y., & Myers, A. C. (2019). Abstraction-safe effect handlers via tunneling. In Proceedings of POPL. 