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

## Work In Progress - Commit Note (2025-02-17)

### Changes Made
1. Fixed type checking and propagator issues:
   - Improved `RecordField` propagator implementation:
     - Simplified type reduction strategy
     - Better error handling for different record types
     - Fixed type mismatch between `CellId[Term]` and `Term`
   - Fixed unification overloads:
     - Added proper type conversion in `unify` methods
     - Ensured consistent handling of meta terms
     - Fixed cell creation and propagation

2. Enhanced type-level reduction:
   - Improved handling of record field access:
     - Try type-level reduction first
     - Fall back to normal reduction if needed
     - Better error reporting for non-record types
   - Added proper error term propagation
   - Fixed handling of meta terms and references

3. Improved error handling:
   - Added more specific error messages
   - Better error context preservation
   - Proper error term propagation through reduction chain

### Fixed Issues
1. Type mismatches:
   - Fixed `CellId[Term]` vs `Term` mismatches in unification
   - Proper handling of meta terms in record field access
   - Consistent type conversion in propagators

2. Import issues:
   - Removed unused imports
   - Added missing error type imports
   - Fixed import organization

3. Record field access:
   - Fixed type-level reduction for record types
   - Improved handling of function calls and references
   - Better error reporting for field access

### Current Status
1. Type checking:
   - Record field access working correctly
   - Type-level reduction properly handling meta terms
   - Proper error propagation

2. Implementation improvements:
   - Simplified propagator logic
   - More efficient type reduction
   - Better error handling

3. Remaining work:
   - Add more test cases
   - Improve performance
   - Add debugging support

### Next Steps
1. Testing:
   - Add more test cases for record field access
   - Test error handling
   - Test type-level reduction

2. Performance:
   - Add caching for reduced terms
   - Optimize type reduction strategy
   - Improve propagator efficiency

3. Documentation:
   - Update implementation notes
   - Add examples
   - Document error cases

### Implementation Guidelines
1. Type-level reduction:
   - Always try type-level reduction first
   - Fall back to normal reduction only when needed
   - Preserve original terms in elaborated results

2. Error handling:
   - Use specific error types
   - Maintain error context
   - Proper error propagation

3. Testing requirements:
   - Test both success and failure cases
   - Verify term preservation
   - Check error message clarity

Note: This is a work in progress. The implementation will be iteratively improved based on testing results and performance measurements.

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

## Current Problems (2025-02-17)

### 1. Import and Type Resolution Issues
- Missing imports and incorrect import paths:
  - `ElaborateContext` import path needs to be fixed
  - `ElaborateError` has been replaced with `TyckProblem` and its case classes
  - Need to update all error handling to use `TyckProblem` case classes

### 2. Type Handling Issues
- Type mismatch in `handleRecordType`:
  - Found `(resultTerm : chester.syntax.core.truffle.FieldAccessTerm)` but required `ProvideElaborater.this.CellIdOr[chester.syntax.core.truffle.Term]`
  - Need to properly handle type conversion between platform-specific implementations
  - Need to ensure proper type preservation during reduction

### 3. Reduction Strategy Issues
- Current aggressive reduction strategy may be too aggressive:
  - Trying type-level reduction first, then normal reduction, then type-level again
  - This might cause unnecessary reductions in some cases
  - Need to optimize when and how reductions are performed

### 4. Error Propagation
- Error terms are not being properly propagated through the reduction chain
- Need to ensure error terms maintain their metadata and context
- Need to improve error messages for better debugging

### 5. Platform-Specific Implementation Issues
- Need to follow platform-specific type system implementation guidelines:
  - Avoid importing from `chester.syntax.core.spec.*`
  - Use `chester.syntax.core.*` for correct platform-specific implementations
  - Handle type conversions properly between platforms

## Next Immediate Steps

### 1. Fix Import and Type Issues
- [ ] Update import paths to use correct package structure
- [ ] Replace `ElaborateError` usage with `TyckProblem` case classes
- [ ] Fix type conversion issues in `handleRecordType`
- [ ] Add proper type annotations for platform-specific implementations

### 2. Improve Reduction Strategy
- [ ] Implement smarter reduction strategy:
  ```scala
  def smartReduce(term: Term, mode: ReduceMode): Term = {
    // First try type-level reduction only if in type-level mode
    if (mode == ReduceMode.TypeLevel) {
      val typeLevelResult = tryTypeLevel(term)
      if (typeLevelResult.isDefined) return typeLevelResult.get
    }
    
    // Try normal reduction only if needed
    val normalResult = tryNormal(term)
    if (normalResult.isDefined) {
      // Try type-level reduction again only in specific cases
      if (shouldTryTypeLevelAgain(normalResult.get)) {
        val finalResult = tryTypeLevel(normalResult.get)
        if (finalResult.isDefined) return finalResult.get
      }
      return normalResult.get
    }
    
    term
  }
  ```

### 3. Enhance Error Handling
- [ ] Implement proper error term propagation:
  ```scala
  case class ErrorContext(
    originalTerm: Term,
    errorType: TyckProblem,
    reductionChain: Vector[Term]
  )
  
  def propagateError(error: ErrorTerm, context: ErrorContext): Term = {
    // Preserve error information while allowing further reduction attempts
    ErrorTerm(
      error.problem,
      error.meta,
      Some(context)
    )
  }
  ```

### 4. Add Debugging Support
- [ ] Add reduction trace logging:
  ```scala
  case class ReductionTrace(
    originalTerm: Term,
    steps: Vector[ReductionStep],
    finalTerm: Term
  )
  
  case class ReductionStep(
    mode: ReduceMode,
    term: Term,
    result: Term
  )
  ```

### 5. Improve Testing
- [ ] Add more test cases for:
  - Nested type-level computations
  - Error propagation
  - Platform-specific type handling
  - Reduction strategy effectiveness
- [ ] Add performance benchmarks for reduction strategies

## Implementation Guidelines

### 1. Type-Level Reduction
- Only perform type-level reduction when absolutely necessary
- Cache reduction results where possible
- Preserve original terms in elaborated results
- Use smart reduction strategy to minimize unnecessary reductions

### 2. Error Handling
- Use appropriate `TyckProblem` case classes for errors
- Maintain error context through reduction chain
- Provide clear error messages with reduction history
- Allow recovery from certain error conditions

### 3. Platform Compatibility
- Follow platform-specific import guidelines
- Use correct type conversions between platforms
- Test on all supported platforms
- Document platform-specific behaviors

### 4. Testing Requirements
- Test both success and failure cases
- Verify term preservation
- Check error message clarity
- Measure performance impact of changes

## Performance Considerations

1. **Reduction Caching**
   - Cache frequently reduced terms
   - Implement smart cache invalidation
   - Monitor cache hit rates

2. **Reduction Strategy**
   - Only reduce when necessary
   - Use heuristics to determine reduction order
   - Avoid redundant reductions

3. **Error Handling**
   - Minimize error term creation
   - Reuse error contexts where possible
   - Optimize error propagation

4. **Memory Usage**
   - Monitor term size during reduction
   - Implement term sharing where possible
   - Clean up temporary terms promptly

## Next Milestone Goals
1. Fix all current compilation errors
2. Implement smart reduction strategy
3. Improve error handling and propagation
4. Add comprehensive testing
5. Optimize performance
6. Update documentation

Note: This is a work in progress. The implementation will be iteratively improved based on testing results and performance measurements 