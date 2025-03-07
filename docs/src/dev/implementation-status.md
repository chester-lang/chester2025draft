# Chester Implementation Status

This document provides practical information about what's currently implemented in Chester, helping developers understand what they can use and what they should avoid.

## Core Type System

### Available Type Features

| Feature | Status | Notes |
|---------|--------|-------|
| Basic Types | ✅ | `Integer`, `String` and basic primitives |
| Function Types | ✅ | Function definitions and calls |
| Records | ✅ | Record types with fields |
| Union Types | ✅ | Using the `|` operator (`Integer | String`) |
| Intersection Types | 🟡 | Internal use only, no syntax yet, [unimplemented case](intersection-type-unification.md) |
| Generic Types | ✅ | Type parameters (`List[T]`) |

### Type Checker Implementation

The type checker is built on a propagator network (constraint-based system):

1. **Implemented Cases**:
   - Basic type unification
   - Union type handling (including subtyping)
   - List types and records

2. **Partially Implemented**:
   - Intersection types are used internally (e.g., for integer literals) but have an unimplemented case:
     ```scala
     // In TyckPropagator.scala (line 334):
     case (Intersection(_, _), Intersection(_, _)) => ???
     ```
     For details on this issue and proposed solution, see [Intersection Type Unification](intersection-type-unification.md).

3. **Not Yet Implemented**:
   - No explicit syntax for declaring intersection types
   - Limited handling of complex union and intersection type combinations

## Built-in Types and Scope

Only a minimal set of types is currently available in the built-in scope:

- **✅ Available**: `Integer`, `String`, `List`, basic primitives
- **❌ Not Available**: `NaturalType`, `IntType`, `UIntType` (defined in code but not in scope)

When writing tests or examples, use only the available types.

## Package System Status

The package/module system is minimal:

- **✅ Implemented**: Basic module declaration syntax
- **❌ Not Implemented**: Actual package loading, resolution, or dependencies

The `module` declarations and package folders exist but are effectively stubs.

## Development Guidelines

1. **Testing Type Checker Changes**:
   - Use only `Integer`, `String` and other confirmed built-in types
   - For intersection types, test through integer literals (internal implementation)
   - Verify both success and failure cases

2. **Avoid Depending On**:
   - Package/module system features
   - Unimplemented types like `NaturalType`
   - Explicit intersection type syntax

3. **Focus Areas for Contribution**:
   - Complete the unimplemented intersection type case in `TyckPropagator.scala` (see [implementation note](intersection-type-unification.md))
   - Improve union type handling for complex cases
   - Extend test coverage for the type checker

4. **Implementation Plans**:
   - For a comprehensive roadmap of type system improvements, see [Type System Improvements](type-system-improvements.md).
 