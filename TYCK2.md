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

## Implementation Notes

### Key Design Decisions

1. **Reduction Context Simplicity**
   - Current `ReduceContext` is intentionally minimal
   - No state preservation needed between reductions
   - Each reduction is independent during type checking
   - Future extensibility possible without breaking changes

2. **Field Access Implementation**
   ```scala
   // Keep original term in elaboration result
   val resultTerm = FieldAccessTerm(recordTerm, fieldName, ...)
   // Only reduce internally for type checking
   val reducedRecordTy = NaiveReducer.reduce(recordTy, ReduceMode.TypeLevel)
   ```
   - Original terms preserved in elaborated result
   - Type-level reduction only used internally
   - Field checking done on reduced type
   - Clean separation between elaboration and type checking

3. **Type-Level Reduction Control**
   ```scala
   case ReduceMode.TypeLevel => retTy match {
     case Type(_, _) => r.reduce(substitutedBody)
     case _ => substitutedBody
   }
   ```
   - Strict control over when reduction happens
   - Only reduces type-level computations
   - Preserves original terms in other cases
   - Maintains clean elaboration results

4. **Let/Def Statement Handling**
   - No reduction during elaboration
   - Original terms preserved in bindings
   - Type checking uses reduction only when needed
   - Clean separation of concerns

### Implementation Invariants

1. **Term Preservation**
   - Original terms MUST be preserved in elaborated results
   - No reduction during elaboration phase
   - Reduction only used internally for type checking
   - Source code structure maintained exactly

2. **Reduction Control**
   - Type-level reduction ONLY in two places:
     1. Type equality checking in unification
     2. Field access checking on type-level terms
   - All other cases MUST preserve original terms
   - No speculative reduction

3. **Clean Separation**
   - Elaboration phase: transforms source to core terms
   - Type checking phase: verifies types using reduction
   - No mixing of concerns between phases
   - Clear boundaries for each operation

4. **Error Reporting**
   - Error messages use original, unreduced terms
   - Source locations preserved
   - Error context matches source code exactly
   - No reduced terms in error messages

These design decisions ensure that the implementation maintains clean elaboration results while still supporting type-level computation where needed.

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