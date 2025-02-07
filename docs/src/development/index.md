# Development Memo

## Platform-Specific Type System Implementation

Chester implements its type system differently for different platforms (JVM and JS). To handle this correctly:

### Import Guidelines

1. **DO NOT** import from `chester.syntax.core.spec.*`
   - The spec package contains trait definitions that are reexported through platform-specific implementations
   - These traits are internal implementation details

2. **DO NOT** import directly from `chester.syntax.core.simple` or `chester.syntax.core.truffle`
   - These are platform-specific implementations that should be accessed through the core package
   - Direct imports can lead to type mismatches and cross-platform compatibility issues

3. **DO** use `import chester.syntax.core.*`
   - This will give you the correct platform-specific implementations
   - Ensures consistent type handling across JVM and JS platforms

### Example

```scala
// INCORRECT
import chester.syntax.core.spec.*
import chester.syntax.core.simple.{BlockTerm, FCallTerm}

// CORRECT
import chester.syntax.core.*
```

### Pattern Matching and Type Usage

1. Use concrete types without suffixes for pattern matching:
```scala
// CORRECT
case t: BlockTerm => {
  val reducedStatements = t.statements.map(stmt => r.reduce(stmt))
  val reducedResult = r.reduce(t.result)
  BlockTerm(reducedStatements, reducedResult, t.meta)
}

// INCORRECT
case t: BlockTermC[Term] => {
  // Don't use *C suffix traits
}
```

2. **DO NOT** use trait versions with suffixes (e.g., `*C`, `*T`):
```scala
// INCORRECT
val reducedStatements = t.statements.map { case stmt: StmtTermT[Term] =>
  r.reduce(stmt).asInstanceOf[StmtTermT[Term]]
}

// INCORRECT
case t: BlockTermC[Term] => // Don't use *C suffix

// CORRECT
val reducedStatements = t.statements.map(stmt => r.reduce(stmt))
```

The codebase provides implicit convert functions for these cases, so explicit type annotations with trait suffixes are unnecessary.

### Why This Matters

- Using concrete types ensures cross-platform compatibility
- The convert functions handle type conversions safely and efficiently
- Avoiding trait suffixes makes the code more maintainable
- This approach leverages the type system to catch potential platform-specific issues at compile time 

## Type Checking and Reduction Strategy

### Preserve Original Terms

1. **Keep Original Form**
   - Always preserve the original, unreduced form of terms in the elaborated output
   - Reduced forms should only be used internally during type checking when absolutely necessary
   - Never expose reduced terms in error messages, semantic analysis, or any user-facing output

2. **When to Reduce**
   - Only reduce terms when type checking requires it, such as:
     - Field access on record types (to see the actual record structure)
     - Function application at the type level (to evaluate type-level functions)
     - Pattern matching on constructors
   - Keep the reduction scope as narrow as possible

```scala
// CORRECT: Only reduce when needed for type checking
case DotCall(recordExpr, fieldExpr, _, _) =>
  val recordTy = newType
  val recordTerm = elab(recordExpr, recordTy, effects)
  // Only reduce if needed to check field access
  val reducedRecordTerm = recordTerm match {
    case _: RecordStmtTerm => recordTerm  // Already a record, no need to reduce
    case _ => Reducer.reduce(recordTerm)   // Reduce only if needed
  }

// INCORRECT: Don't reduce unnecessarily
case expr =>
  val reduced = reducer.reduce(expr)  // Don't reduce everything
  // ...
```

3. **Benefits**
   - Preserves source code structure for better error messages
   - Maintains traceability back to original code
   - Improves debugging experience
   - Keeps semantic information intact
   - Makes refactoring and code analysis more accurate

4. **Implementation Guidelines**
   - Use lazy reduction strategies
   - Cache reduced forms when they must be computed
   - Keep reduction local to type checking logic
   - Never modify the original AST structure
   - Return original terms in elaboration results

### Platform-Specific Type System Implementation

#### Type System Architecture

1. **Platform Independence**
   - The type system is designed to work consistently across JVM and JS platforms
   - Core types are defined as traits in `chester.syntax.core.spec`
   - Platform-specific implementations are provided in `simple` and `truffle` packages
   - Users should never need to interact with platform-specific implementations directly

2. **Type Conversion**
   - Implicit conversions are provided for seamless type handling
   - Let the compiler handle type conversions through the core package
   - Avoid explicit type casts or platform-specific type annotations

3. **Error Handling**
   - Type errors should be platform-agnostic
   - Error messages should reference source code types, not internal representations
   - Use `TyckResult` for consistent error reporting across platforms

#### Testing Guidelines

1. **Platform-Specific Tests**
   - Write tests that work consistently across platforms
   - Avoid platform-specific type assertions
   - Use `import chester.syntax.core.*` in test files
   - Don't directly test platform-specific implementations

2. **Type Checking Tests**
   - Test type checking without assuming implementation details
   - Verify error messages reference source code types
   - Ensure tests pass on both JVM and JS platforms

3. **Common Test Issues**
   - Using platform-specific imports
   - Assuming specific type implementations
   - Not handling both platforms in test cases
   - Direct manipulation of internal type representations

### Lazy Reduction Approach

The type checker should avoid unnecessary term reduction during elaboration. Only reduce terms when absolutely necessary:

1. **Default Behavior**: Try to type check without reduction first
2. **Reduction Triggers**:
   - Field access on record types (to see the actual record structure)
   - Function application at the type level (to evaluate type-level functions)

### Example

```scala
// CORRECT: Check without reduction first
case DotCall(recordExpr, fieldExpr, _, _) =>
  val recordTy = newType
  val recordTerm = elab(recordExpr, recordTy, effects)
  // Only reduce if needed to check field access
  
// INCORRECT: Reducing everything
case expr =>
  val reduced = reducer.reduce(expr)  // Don't reduce unnecessarily
  // ...
```

### Why This Matters

- Improves performance by avoiding unnecessary computation
- Preserves original term structure when possible
- Makes type checking more predictable
- Keeps error messages more relevant to source code 

## Testing Strategy

### Running Tests Efficiently

1. **Always Run Specific Module Tests**
   ```bash
   # CORRECT: Run tests for specific module
   sbt "semantic/test"
   sbt "reader/test"
   
   # For running all JVM tests
   sbt "rootJVM/test"    # Note: Will be slow if Scala.js and Scala Native are enabled
   
   # INCORRECT: Never run all tests with plain 'sbt test'
   sbt test        # VERY slow - will run Scala.js and Scala Native tests which take a long time to compile
   sbt testOnly    # Too broad
   ```

2. **Further Narrow Test Scope When Possible**
   ```bash
   # Even better: Run specific test class
   sbt "semantic/testOnly chester.tyck.LazyReductionTest"
   ```

3. **Why This Matters**
   - Faster feedback loop
   - More focused debugging
   - Prevents unnecessary test runs
   - Saves CI/CD resources
   - Avoids cross-platform compilation overhead when not needed
   - Scala.js and Scala Native tests require full recompilation and are very slow 