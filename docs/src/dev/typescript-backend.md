# TypeScript Backend Implementation

## Overview

This document outlines the implementation plan for a TypeScript backend in the Chester compiler. The backend will take type-checked Chester code and generate equivalent TypeScript code, leveraging the existing JavaScript AST infrastructure.

## Goals

- Create a TypeScript code generator that uses the existing JavaScript AST
- Ensure proper handling of Chester's type system in TypeScript output
- Support TypeScript-specific features like interfaces, type annotations, and generics
- Maintain type safety between Chester and TypeScript

## Current Status

- The JavaScript AST (`compiler/shared/src/main/scala/chester/targets/js/AST.scala`) already includes TypeScript-specific nodes
- We need to implement the transformation from Chester's type-checked AST to TypeScript AST
- We need to implement a code generator for TypeScript

## Implementation Tasks

### 1. TypeScript AST Enhancements

While the existing JavaScript AST includes TypeScript nodes, we may need to extend it with additional TypeScript-specific features:

- Ensure all TypeScript type annotations are properly represented
- Add support for TypeScript-specific syntax like `readonly`, `namespace`, etc.
- Implement TypeScript module system support

### 2. Chester to TypeScript Type Mapping

Create a mapping between Chester's type system and TypeScript types:

| Chester Type | TypeScript Type |
|--------------|----------------|
| Integer      | number         |
| String       | string         |
| Boolean      | boolean        |
| Unit         | void           |
| Type         | any            |
| Function     | Function       |
| Record       | interface      |
| Union        | union type     |
| Effect       | (see below)    |

### 3. Effect System Handling

For Chester's effect system, we have several options for TypeScript representation:

1. **Type-based approach**: Represent effects as part of function types
   ```typescript
   type IOEffect<T> = T & { readonly __io: unique symbol };
   type StateEffect<T> = T & { readonly __state: unique symbol };
   ```

2. **Comment-based approach**: Use TypeScript comments to document effects
   ```typescript
   /** @effect IO */
   function print(message: string): void { ... }
   ```

3. **Runtime checking**: Implement runtime effect checking in TypeScript
   ```typescript
   function withEffects<T>(fn: () => T, effects: Effect[]): T { ... }
   ```

The recommended approach is #1, as it provides compile-time checking in TypeScript.

### 4. Code Generator Implementation

Implement a TypeScript code generator with the following components:

- **AST Transformer**: Convert Chester AST to TypeScript AST
- **Type Transformer**: Convert Chester types to TypeScript types
- **Effect Transformer**: Handle effect annotations
- **Code Emitter**: Generate TypeScript code from the AST

### Code Emitter

Transforms the TypeScript AST into a valid TypeScript code string.

## Implementation Plan

The backend involves several key components:

### 1. AST Definition (`js.AST.scala`)

Defines the structure of the target JavaScript/TypeScript Abstract Syntax Tree (AST).

### 2. AST Transformer

Converts the type-checked Chester **core AST** (`chester.syntax.core.Term`) into the target `js.AST`.
*   **Node Mapping**: Map each relevant `core.Term` node to its equivalent `js.AST` node(s).
*   **Type Mapping**: Translate Chester types (including unions, records) to TypeScript types.
*   **Effect Transformer**: Handle effect annotations (potentially via comments or specific code structures).

### 3. Code Emitter

Transforms the `js.AST` into a valid TypeScript code string.

## Current Status (as of YYYY-MM-DD)

*   The basic AST node definitions (`js.AST.scala`) exist in `compiler/shared/src/main/scala/chester/targets/js/`.
*   A placeholder backend object (`Backend.scala`) has been created in the same directory (`chester.targets.js` package). It expects `chester.syntax.core.Term` as input and contains basic transformation logic for some nodes, but needs refinement and completion.
*   The AST Transformer logic within `Backend.scala` is incomplete and requires verification against the actual `core.Term` structure.
*   The detailed Code Emitter logic **has not yet been implemented**.
*   The integration of this backend into the main compilation pipeline or test infrastructure needs to be done.

## Challenges

*   Mapping Chester's type system (including union types, structural types) to TypeScript's type system.
*   Handling effects and ensuring the generated code respects them (perhaps via comments or specific function signatures).

### 5. Integration with Compiler Pipeline

- Add a TypeScript target option to the compiler
- Integrate the TypeScript backend with the existing compilation pipeline
- Ensure proper error handling and reporting

## Implementation Plan

1. **Phase 1: Basic TypeScript Generation**
   - Implement basic AST transformation
   - Handle primitive types and simple functions
   - Generate valid TypeScript code without effects

2. **Phase 2: Advanced Type Features**
   - Implement generics
   - Handle record types and interfaces
   - Support union and intersection types

3. **Phase 3: Effect System Integration**
   - Implement effect type representation
   - Handle effect propagation in TypeScript
   - Ensure effect safety in generated code

4. **Phase 4: Optimization and Refinement**
   - Optimize generated TypeScript code
   - Improve readability of output
   - Add source mapping for debugging

## Example Transformation

### Chester Input:
```
// Function with an effect
def print(message: String) : Unit / IO = ()

// Function that uses the effect
def hello() : Unit / IO = {
  print("Hello")
}

// Pure function (no effects)
def pure() : Integer = 123
```

### TypeScript Output:
```typescript
// Function with an effect
function print(message: string): IOEffect<void> {
  // Implementation
  return undefined as IOEffect<void>;
}

// Function that uses the effect
function hello(): IOEffect<void> {
  print("Hello");
  return undefined as IOEffect<void>;
}

// Pure function (no effects)
function pure(): number {
  return 123;
}

// Effect type definitions
type Effect = { readonly __effect: unique symbol };
type IOEffect<T> = T & { readonly __io: unique symbol };
type StateEffect<T> = T & { readonly __state: unique symbol };
```

## Success Criteria

- The TypeScript backend can generate valid TypeScript code from Chester programs
- The generated TypeScript code maintains the type safety of the original Chester code
- Effects are properly represented and checked in the TypeScript output
- The TypeScript code is readable and follows TypeScript best practices 