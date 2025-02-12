# Type Checker Implementation Plan (tyck2)

## Goal
Implement basic dependent typing features to support:
- Types as first-class values
- Type-level computation
- Type aliases and references

Example that should work:
```chester
record A(a: Integer);
def idType(x: Type): Type = x;
let aT = A;
def getA(x: A): Integer = x.a;
def getA1(x: aT): Integer = x.a;
```

## Core Requirements
1. Evaluation during type checking
   - Evaluate type-level expressions
   - Handle references to type values (like `aT`)
   - Support basic type-level functions (like `idType`)

2. Type equality
   - Structural equality for types
   - Handle evaluated type expressions
   - Compare type references with their definitions

## Implementation Plan

### Phase 1: Types as Values
1. Modify `Type.scala` to:
   - Make types first-class values
   - Support type-level references
   - Basic type equality

2. Add evaluation to type checking:
   - Evaluate type expressions during checking
   - Handle let-bound type values
   - Basic type-level computation

### Phase 2: Records and References
1. Support record types:
   - Record type definitions
   - Field access
   - Type equality for records

2. Handle type aliases:
   - Let-bound types
   - Type reference resolution

## Testing
Focus on the example case:
- Record type definition
- Type-level functions
- Type aliases
- Type equality with aliases 