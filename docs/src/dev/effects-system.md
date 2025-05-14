# Effect System Design

NOTE THAT THIS DOCUMENT IS OUTDATED AS RELEVANT CODE IS BEING REWRITTEN

## Overview

The Chester language includes a built-in effect system that enables tracking and controlling side effects. This document outlines the design and implementation of this system.

## Core Concepts

- **Effect**: A built-in type in the language that represents a capability to perform a specific kind of operation.
- **Effect Values**: Built-in values of type `Effect` (e.g., `IO`, `State`, `Exception`).
- **Effect Requirements**: Functions declare which effects they may use with the `/ Effect` syntax.
- **Effect Propagation**: Effect requirements automatically propagate up the call chain.
- **Pure Functions**: Functions without effect annotations are pure (no side effects).

## Syntax

### Function Declaration with Effects

```
def functionName(args): ReturnType / EffectType = body
```

Examples:
```
// Function with IO effect
def print(message: String): Unit / IO = ()

// Function with multiple effects
def readAndWrite(): String / (IO & State) = { ... }
```

### Effect Propagation

When a function calls another function with effects, those effects must be declared in the caller's signature:

```
def hello(): Unit / IO = {
  print("Hello")  // print has IO effect, so hello needs IO too
}
```

## Built-in Effects

The language provides several built-in effects:

- **IO**: Input/output operations (file I/O, console I/O, etc.)
- **State**: Mutable state operations
- **Exception**: Operations that may throw exceptions
- **NonTermination**: Operations that may not terminate

## Implementation Notes

The effect system is implemented through:

1. **Type Checking**: Functions are verified to declare all effects they use.
2. **Effect Propagation**: Functions automatically require any effects used by functions they call.
3. **Effect Handling**: The runtime system ensures effects are properly handled.

## Future Extensions

Potential future extensions include:

- User-defined effects
- Effect polymorphism
- Effect inference
- Effect handlers for controlling effect execution

## Example Usage

```
// Function with an IO effect requirement
def print(message: String): Unit / IO = ()

// This function automatically inherits the IO effect
def hello(): Unit / IO = {
  print("Hello")
}

// Pure function with no effects
def pure(): Integer = 123
``` 