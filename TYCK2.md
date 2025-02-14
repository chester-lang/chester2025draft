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

Example:
```chester
def idType(x: Type): Type = x;
let aT = idType(A);  // Use TypeLevel mode to reduce to A
```

### Step 2: Type-Level Let Bindings
Enhance let binding handling:
- Use TypeLevel reduction mode for type bindings
- Maintain original terms in core representation

Example:
```chester
let aT = A;
let bT = aT;  // Use TypeLevel mode to resolve to A
```

### Step 3: Field Access with Type Reduction
Connect field access checking with reduction:
- Use TypeLevel mode for type references in field access
- Handle field access on reduced type values

Example:
```chester
record A(a: Integer);
let aT = A;
def getA1(x: aT): Integer = x.a;  // Use TypeLevel mode to reduce aT to A
```

## Testing Strategy
Test both reduction modes:

1. Type-level reduction:
```chester
def idType(x: Type): Type = x;
def id2(x: Type): Type = idType(x);
let aT = id2(A);  // Should reduce to A in TypeLevel mode
```

2. Let binding reduction:
```chester
let aT = A;
let bT = aT;  // Should resolve to A in TypeLevel mode
let x = 1;    // Should not over-reduce in TypeLevel mode
```

3. Field access with reduction:
```chester
record A(a: Integer);
def idType(x: Type): Type = x;
let aT = idType(A);
def getA1(x: aT): Integer = x.a;  // Should work with TypeLevel mode
```

## Implementation Thinking and Pitfalls

### When to Use Type-Level Reduction

1. **DO** use type-level reduction for:
   - Type equality checking in unification
   - Field access on type-level computations
   - Evaluating type expressions

2. **DO NOT** use type-level reduction for:
   - Initial elaboration of let/def bindings
   - Places where original terms should be preserved
   - Regular value-level computations

### Common Pitfalls

1. **Over-reduction**
   ```chester
   let aT = A;
   let bT = aT;  // WRONG: Don't reduce to A during elaboration
                 // RIGHT: Keep as aT, only reduce when needed for type checking
   ```

2. **Loss of Original Terms**
   ```chester
   def idType(x: Type): Type = x;
   let aT = idType(A);
   def getA1(x: aT): Integer = x.a;  // Keep idType(A) in elaborated result
                                     // Only reduce to A when checking field access
   ```

3. **Incorrect Reduction Timing**
   - Reducing too early loses source code structure
   - Reducing too late causes type checking errors
   - Reduction should happen during type checking, not elaboration

### Design Principles

1. **Term Preservation**
   - Keep original terms in core representation
   - Preserve source code structure for better error messages
   - Allow for future transformations

2. **Lazy Reduction**
   - Only reduce when necessary for type checking
   - Use type-level reduction mode to control evaluation
   - Maintain balance between preservation and correctness

3. **Clear Reduction Boundaries**
   - Separate elaboration from type checking
   - Use reduction modes to control evaluation scope
   - Keep reduction logic centralized in reducer 