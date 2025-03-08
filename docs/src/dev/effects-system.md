# Algebraic Effects in Chester

## Introduction

Algebraic effects are a programming language feature that provides a structured way to handle computational effects, such as I/O, state, exceptions, non-determinism, and other side effects. They offer several advantages over traditional approaches to effect handling:

1. **Separation of concerns**: Effect handlers are separated from the code that uses effects
2. **Composability**: Different effects can be combined and composed easily
3. **Type safety**: Effects can be tracked at the type level
4. **Local reasoning**: Code can be understood without knowing about all possible effect handlers

This document outlines the design and implementation of algebraic effects in Chester.

## Current Implementation Status

Chester currently has a partial implementation of the effects system with the following components:

### Core Data Structures

1. **`Effects` class** (in `syntax/shared/src/main/scala/chester/syntax/core/simple.scala`):
   - Represents a set of effects as a map from `LocalV` (effect identifiers) to `Term` (effect values)
   - Has an `Empty` constant for representing no effects

2. **`EffectsCell` trait** (in `semantic/shared/src/main/scala/chester/tyck/ElaboraterCommon.scala`):
   - Extends `Cell[Effects]`
   - Provides a `requireEffect` method to add a new effect to the cell
   - Has two implementations:
     - `DynamicEffectsCell`: A mutable cell that can accumulate effects
     - `FixedEffectsCell`: An immutable cell with predefined effects

3. **`FunctionType`** (in `syntax/shared/src/main/scala/chester/syntax/core/simple.scala`):
   - Includes an `effects: EffectsM` parameter to track effects associated with a function

### Effect Checking in Function Calls

In `semantic/shared/src/main/scala/chester/tyck/ElaboraterFunctionCall.scala`:

1. **Effect Collection**: The `collectArgumentEffects` method gathers effects from all arguments in a function call
2. **Effect Checking**: The `checkEffects` method verifies that all effects from arguments are included in the function's effects
3. **Error Reporting**: The `UnauthorizedEffectError` is reported when a function call uses an effect not declared by the function

## Planned Enhancements

To complete the algebraic effects system, we need to implement:

### 1. Effect Declaration Syntax

Allow declaring effects in function types with syntax like:

```
def myFunction() : ReturnType / Effect1, Effect2 {
  // function body that may use Effect1 and Effect2
}
```

### 2. Effect Handler Syntax

Implement syntax for defining effect handlers:

```
handler MyEffect {
  effect operation1(arg1: Type1): ReturnType1 {
    // handler implementation
  }
  
  effect operation2(arg2: Type2): ReturnType2 {
    // handler implementation
  }
}
```

### 3. Effect Propagation Rules

Implement the following rules for effect propagation:

- **Function Application**: When a function is called, its effects are propagated to the caller
- **Effect Handlers**: A handler can capture and handle specific effects, preventing their propagation
- **Effect Subsumption**: If function `f` requires effects `E1`, then it can be used in a context requiring effects `E2` where `E1 ⊆ E2`

### 4. Type Checking for Effects

Enhance type checking to:

- Verify that functions only use effects they declare
- Ensure that all effects are properly handled or propagated
- Check that effect handlers properly implement the operations they claim to handle

### 5. Effect Inference

Implement inference for effects to reduce the annotation burden:

- Infer effects for unannotated functions based on their bodies
- Propagate effects automatically through the call graph
- Provide clear error messages when effect constraints are violated

## Implementation Plan

1. **First Phase**: Implement basic effect checking rules (current implementation)
   - Track effects in function types ✓
   - Check effects in function calls ✓
   - Propagate effects from callee to caller

2. **Second Phase**: Implement effect handlers
   - Define syntax for effect handlers
   - Implement handler application
   - Implement resumption

3. **Third Phase**: Complete the system with advanced features
   - Effect polymorphism
   - Effect inference
   - Integration with higher-order functions

## Testing Strategy

1. **Unit Tests**:
   - Test effect propagation in simple function calls
   - Test effect checking with handlers
   - Test error cases for unauthorized effects

2. **Integration Tests**:
   - Test complex programs with multiple effects
   - Test interaction between different effect handlers
   - Test higher-order functions with effects

3. **Example Programs**:
   - Implement standard examples like state, exceptions, non-determinism
   - Create examples showcasing effect composition
   - Develop practical uses of algebraic effects

## Conclusion

The algebraic effects system in Chester aims to provide a powerful and flexible way to handle computational effects with strong static guarantees. This document serves as a roadmap for implementing and enhancing this feature. 