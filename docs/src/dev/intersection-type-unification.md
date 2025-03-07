# Intersection Type Unification Implementation Note

> **Note**: This is a focused implementation note. For the broader context and roadmap of type system improvements, please refer to the [Type System Improvements](type-system-improvements.md) document.

## Current Issue

In `TyckPropagator.scala` (line 334), there's an unimplemented case for unifying two intersection types:

```scala
case (Intersection(_, _), Intersection(_, _)) => ???
```

This prevents proper type checking when two intersection types need to be compared for compatibility. While Chester doesn't yet support declaring intersection types in the source code with an explicit syntax, they are used internally (e.g., for handling integer literals), making this a critical part of the type system.

## Implementation Plan

### 1. The Solution

The proposed implementation for this case is:

```scala
case (Intersection(types1, _), Intersection(types2, _)) =>
  // For intersection types to be compatible, each type from the second intersection
  // must be compatible with at least one type from the first intersection
  types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))
```

This implements the semantic meaning of intersection types where:
- If `A & B` is compatible with `C & D`, then
- `C` must be compatible with either `A` or `B`, and
- `D` must be compatible with either `A` or `B`

### 2. Code Location

The change will be made in the `tryUnify` method in `TyckPropagator.scala`, around line 334.

### 3. Testing Approach

Since explicit intersection types aren't supported in the syntax yet, we'll test through integer literals which use intersection types internally:

```chester
// Test through integer literals, which use intersection types internally
let a: Integer = 42;  // 42 has an intersection type internally
```

We'll focus on verifying that integer literals continue to work correctly with type checking.

## Expected Behavior

After this change:
1. Integer literals should continue to work correctly
2. The type checker will have more complete handling of intersection types
3. The `???` placeholder will be replaced with an actual implementation
4. We'll be better prepared for future expansion of intersection type support

## Minimal Change Scope

This change is deliberately minimal:
- Only addressing the unimplemented case
- Not adding any new features beyond what's needed
- Not changing any existing type checking behavior
- Not modifying the external API or syntax

The change improves internal robustness without introducing new syntax or APIs.

## Implementation Status

⚠️ **Not Yet Implemented**: This change has been documented but not yet implemented in the codebase. The `???` remains in the code as an unimplemented case. 