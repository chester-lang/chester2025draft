# Type Checker Implementation Plan (tyck2)

## Goal
Support type-level computation during type checking by adding type-level reduction mode. Example:
```chester
record A(a: Integer);
def idType(x: Type): Type = x;
let aT = A;
def getA(x: A): Integer = x.a;
def getA1(x: aT): Integer = x.a;
```

## Current System
- `Elaborater.scala`: Type checks and elaborates terms, with support for evaluation during checking
- `Reducer.scala`: Reduces terms to normal form, including function application
- System preserves original terms in core representation unless reduction needed

## Implementation Plan

### Step 1: Add Reduction Modes
Modify `NaiveReducer` to support different reduction modes:
- Add `ReduceMode` enum for controlling reduction strategy
- Make recursion explicit using passed reducer
- Support type-level reduction mode

Code Changes:
```scala
/** Controls how aggressively terms are reduced */
enum ReduceMode {
  case TypeLevel // Only reduce type-level computations
  case Normal    // Normal reduction strategy
}

object NaiveReducer extends Reducer {
  def reduce(term: Term, mode: ReduceMode)(using ctx: ReduceContext, r: Reducer): Term = {
    // ... match on term ...
    case FCallTerm(f, args, meta) =>
      // ... reduce function and args ...
      case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
        val substitutedBody = /* ... */
        mode match {
          case ReduceMode.Normal => r.reduce(substitutedBody)
          case ReduceMode.TypeLevel => retTy match {
            case Type(_, _) => r.reduce(substitutedBody)
            case _ => substitutedBody
          }
        }
  }
}
```

### Step 2: Preserve Original Terms
Key principle: Keep original terms in elaborated results, only reduce internally when needed.

Example:
```chester
let aT = idType(A);  // Keep as idType(A) in elaborated result
let bT = aT;         // Keep as aT in elaborated result
```

### Step 3: Field Access with Type Reduction
Use type-level reduction ONLY during field access checking:
- Keep original term in elaborated result
- Use reduction internally to verify field exists
- Handle field access on reduced type values

Example:
```chester
record A(a: Integer);
let aT = idType(A);
def getA1(x: aT): Integer = x.a;  // Elaborated result keeps aT
                                  // Internally reduce to check field
```

## Testing Strategy
Test both preservation and reduction:

1. Term preservation:
```chester
def idType(x: Type): Type = x;
let aT = idType(A);  // Should stay as idType(A) in elaborated result
```

2. Internal reduction for type checking:
```chester
record A(a: Integer);
let aT = idType(A);
def getA1(x: aT): Integer = x.a;  // Should verify field exists by reduction
                                  // But keep aT in elaborated result
```

## Testing Requirements

### Before Committing Changes
1. ALWAYS run `sbt rootJVM/test` before committing
2. Fix any test failures before proceeding
3. Add new tests for any new functionality
4. Update existing tests when modifying behavior

### Test Cases to Cover
1. **Term Preservation**
   ```chester
   def idType(x: Type): Type = x;
   let aT = idType(A);  // Test elaborated result contains idType(A)
   let bT = aT;         // Test elaborated result contains aT
   ```

2. **Type-Level Reduction**
   ```chester
   record A(a: Integer);
   def idType(x: Type): Type = x;
   let aT = idType(A);
   def getA1(x: aT): Integer = x.a;  // Test field access works
   def getA2(x: idType(A)): Integer = x.a;  // Test direct type-level function application
   ```

3. **Error Cases**
   ```chester
   record A(a: Integer);
   def wrongType(x: Type): Type = B;  // Non-existent type
   let aT = wrongType(A);
   def getA(x: aT): Integer = x.a;  // Should report clear error
   ```

4. **Edge Cases**
   ```chester
   record A(a: Integer);
   def idType(x: Type): Type = x;
   let aT = idType(A);
   let bT = idType(aT);  // Nested type-level computation
   def getA(x: bT): Integer = x.a;  // Should work correctly
   ```

## Implementation Notes

### When to Use Reduction

Type-level reduction should only happen in two specific places:

1. During type equality checking in `unify` - This is necessary to determine if two types are equal after evaluating any type-level computations.
2. During field access type checking - When checking field access on a type constructed by a type-level function, we need to reduce the type to see its structure.

### Preserving Original Terms

The elaborated result should preserve the original terms whenever possible:

1. In field access (`DotCall`), store the original term in `FieldAccessTerm` while using the reduced type only internally for checking.
2. In let/def bindings, store the original terms without reduction.
3. When adding bindings to the context, use the original unreduced terms.

### Common Pitfalls

1. DO NOT reduce terms during elaboration unless explicitly needed for type checking.
2. DO NOT reflect internal reductions in the elaborated result.
3. DO NOT reduce terms when adding them to the context.
4. DO ensure that type-level reduction is used in `unify` for type equality checking.
5. DO ensure that field access type checking uses type-level reduction while preserving original terms.

### Testing Strategy

When implementing new features:

1. Write tests that verify original terms are preserved in elaborated results
2. Write tests that verify type checking works correctly with type-level computation
3. Write tests that verify field access works with types constructed by type-level functions
4. Write tests that verify type equality checking works with reduced types

## Implementation Thinking and Pitfalls

### When to Use Type-Level Reduction

1. **DO** use type-level reduction ONLY for:
   - Type equality checking in unification
   - Field access checking on type-level terms
   - NEVER in elaborated results

2. **DO NOT** use type-level reduction for:
   - Elaboration of any terms
   - Let/def binding processing
   - Any place where original terms should be kept

### Common Pitfalls

1. **Over-reduction**
   ```chester
   let aT = A;
   let bT = aT;  // WRONG: Don't reduce to A during elaboration
                 // RIGHT: Keep as aT in elaborated result
   ```

2. **Loss of Original Terms**
   ```chester
   def idType(x: Type): Type = x;
   let aT = idType(A);
   def getA1(x: aT): Integer = x.a;  // WRONG: Reducing idType(A) in result
                                     // RIGHT: Keep idType(A), only reduce for checking
   ```

3. **Incorrect Reduction Timing**
   - WRONG: Reducing during elaboration
   - RIGHT: Only reduce during type checking when needed
   - Keep reduction internal to type checker

### Design Principles

1. **Term Preservation**
   - Always keep original terms in elaborated result
   - Never reduce terms just for elaboration
   - Preserve source code structure exactly

2. **Minimal Reduction**
   - Only reduce when immediately necessary
   - Keep reduction internal to type checker
   - Never reduce speculatively

3. **Clear Boundaries**
   - Elaboration never reduces
   - Type checking reduces only when needed
   - Keep original terms everywhere else 