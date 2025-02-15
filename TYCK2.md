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

## Work In Progress - Commit Note (2024-03-21)

### Changes Made
1. Enhanced function call reduction:
   - Added proper argument substitution
   - Implemented two-phase reduction strategy (type-level then normal)
   - Fixed handling of non-function cases

2. Improved field access handling:
   - Added type-level reduction as first attempt
   - Implemented fallback to normal reduction
   - Preserved original terms in elaborated results

3. Enhanced reference resolution:
   - Added proper context handling with `knownMap`
   - Implemented recursive resolution strategy
   - Added support for type definitions

### Known Issues
1. Tests not passing:
   - `NotARecordType` error still occurring in some nested type-level computations
   - Test file: `tests/tyck/type-level-reduction.chester`
   - Specific test: `sameType2(x: idType(A)): Integer = x.a`

2. Current hypothesis:
   - Function call reduction may need to be more aggressive in type-level mode
   - Reference resolution might need to try both reduction modes
   - May need to enhance reduction order in field access checking

### Next Immediate Steps
1. Fix test failures:
   - Add more test cases for nested type-level computations
   - Improve reduction strategy for function calls
   - Add better error reporting for debugging

2. Planned improvements:
   - Add reduction trace for debugging
   - Consider caching reduced terms
   - Implement cycle detection

Note: This is a mid-work commit. Tests are not passing yet, but the core implementation of type-level reduction is in place.

## Current Implementation Status

### Reduction Context
- `ReduceContext.scala`: Provides context for reduction with:
  - `knownMap`: Maps reference IDs to their known values
  - `typeDefinitions`: Maps type definition IDs to their definitions
  - `resolve` method for resolving references to their known values

### Reducer Implementation
- `Reducer.scala`: Implements term reduction with two modes:
  - `ReduceMode.TypeLevel`: For type-level computations
  - `ReduceMode.Normal`: For normal reduction

Key features:
1. Field Access Reduction:
   ```scala
   case FieldAccessTerm(target, field, fieldType, meta) => {
     // Try type-level reduction first
     val reducedTarget = r.reduce(target, ReduceMode.TypeLevel)
     if (reducedTarget != target) {
       FieldAccessTerm(reducedTarget, field, fieldType, meta)
     } else {
       // Fall back to normal reduction
       val normalReducedTarget = r.reduce(target, ReduceMode.Normal)
       FieldAccessTerm(normalReducedTarget, field, fieldType, meta)
     }
   }
   ```

2. Function Call Reduction:
   ```scala
   case FCallTerm(f, args, meta) => {
     // First reduce function and args in current mode
     val reducedF = r.reduce(f, mode)
     val reducedArgs = args.map(calling =>
       Calling(
         calling.args.map(arg => CallingArgTerm(r.reduce(arg.value, mode), r.reduce(arg.ty), arg.name, arg.vararg, arg.meta)),
         calling.implicitly,
         calling.meta
       )
     )
     
     reducedF match {
       case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
         // Substitute args into body
         val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
           telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
             acc.substitute(param.bind, arg.value)
           }
         }
         // Try reducing in current mode first
         val evaluated = r.reduce(substitutedBody, mode)
         if (evaluated != substitutedBody) {
           evaluated
         } else {
           // Try other mode if needed
           val otherMode = mode match {
             case ReduceMode.TypeLevel => ReduceMode.Normal
             case ReduceMode.Normal => ReduceMode.TypeLevel
           }
           r.reduce(substitutedBody, otherMode)
         }
       case _ => 
         // If not a function, try the other reduction mode
         val otherMode = mode match {
           case ReduceMode.TypeLevel => ReduceMode.Normal
           case ReduceMode.Normal => ReduceMode.TypeLevel
         }
         r.reduce(term, otherMode)
     }
   }
   ```

3. Reference Resolution:
   ```scala
   case ref: ReferenceCall => {
     val resolved = ctx.resolve(ref)
     if (resolved != ref) {
       r.reduce(resolved, mode)
     } else {
       ref
     }
   }
   ```

### Type Checking Strategy
1. **Term Preservation**
   - Original terms are preserved in elaborated results
   - Reduction only happens internally during type checking
   - Example:
     ```chester
     let aT = idType(A);  // Keeps idType(A) in result
     ```

2. **Type-Level Reduction**
   - Used in two specific places:
     1. Type equality checking in unification
     2. Field access checking on type-level terms
   - Example:
     ```chester
     def getA1(x: aT): Integer = x.a;  // Reduces aT to A internally
     ```

3. **Reduction Order**
   - Try type-level reduction first for field access
   - For function calls:
     1. Reduce function and args in current mode
     2. Try reducing result in current mode
     3. Fall back to other mode if needed
   - Handle reference resolution recursively

## Testing Requirements

### Test Cases
1. **Term Preservation**
   ```chester
   def idType(x: Type): Type = x;
   let aT = idType(A);  // Test elaborated result contains idType(A)
   ```

2. **Type-Level Reduction**
   ```chester
   record A(a: Integer);
   def idType(x: Type): Type = x;
   let aT = idType(A);
   def getA1(x: aT): Integer = x.a;  // Test field access works
   def getA2(x: idType(A)): Integer = x.a;  // Test direct type-level function application
   ```

3. **Nested Computation**
   ```chester
   let cT = idType(aT);  // Test nested type-level computation
   def getA3(x: cT): Integer = x.a;  // Should work via reduction
   ```

### Implementation Guidelines
1. **Reduction Context**
   - Properly initialize with knownMap and typeDefinitions
   - Ensure reference resolution works correctly
   - Handle cyclic references gracefully

2. **Reducer Implementation**
   - Handle both reduction modes correctly
   - Properly handle function calls with argument substitution
   - Support reference resolution with proper recursion control
   - Maintain mode-specific reduction strategies

3. **Type Checking**
   - Preserve original terms in elaboration
   - Use type-level reduction for type checking
   - Handle error cases gracefully
   - Provide clear error messages for reduction failures

## Current Issues
1. **NotARecordType Error**
   - Still seeing this error in some test cases
   - Current fixes:
     - Improved function call reduction with proper arg substitution
     - Added fallback to normal reduction when type-level fails
     - Enhanced reference resolution
   - Remaining work:
     - Test more complex nested type-level computations
     - Add better error reporting for reduction failures
     - Consider adding reduction trace for debugging

## Next Steps
1. Fix remaining test failures:
   - Focus on nested type-level computation cases
   - Add more test cases for complex scenarios
2. Improve error reporting:
   - Add reduction trace information
   - Provide clearer messages for reduction failures
3. Enhance reduction strategy:
   - Consider caching reduced terms
   - Add cycle detection for recursive types
4. Documentation:
   - Add examples of common reduction patterns
   - Document error cases and their solutions
   - Create troubleshooting guide 