# Development Memo

## Platform-Specific Type System Implementation

Chester implements its type system differently for different platforms (JVM and JS). To handle this correctly:

### Import Guidelines

1. **DO NOT** import from `chester.syntax.core.spec.*`
   - The spec package contains trait definitions that are reexported through platform-specific implementations

2. **DO NOT** import directly from `chester.syntax.core.simple` or `chester.syntax.core.truffle`
   - These are platform-specific implementations that should be accessed through the core package

3. **DO** use `import chester.syntax.core.*`
   - This will give you the correct platform-specific implementations

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