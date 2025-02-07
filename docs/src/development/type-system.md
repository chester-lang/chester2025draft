# Type System Implementation

## Overview

Chester implements a sophisticated type system that works consistently across different platforms (JVM and JS). This guide explains the implementation details and best practices.

## Platform-Specific Implementation

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

## Type Checking and Reduction

### Lazy Reduction Strategy

The type checker should avoid unnecessary term reduction during elaboration:

1. **Default Behavior**: Try to type check without reduction first
2. **Reduction Triggers**:
   - Field access on record types (to see the actual record structure)
   - Function application at the type level (to evaluate type-level functions)
   - Pattern matching on constructors

### Example Implementation

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

## Record Constructor Handling

When handling record constructors, follow these guidelines:

1. **Direct Argument Handling**
   ```scala
   // CORRECT
   val elaboratedArgs = args.zip(fields).map { case (arg, field) =>
     val argTy = toId(field.ty)
     val argTerm = elab(arg, argTy, effects)
     state.addPropagator(Unify(argTy, toId(argTerm), arg))
     argTerm
   }
   RecordConstructorCallTerm(name, elaboratedArgs, meta)

   // INCORRECT - Unnecessary tuple wrapping
   val tupleType = TupleType(fields.map(_.ty))
   val tupleArg = TupleTerm(elaboratedArgs)
   RecordConstructorCallTerm(name, Vector(tupleArg), meta)
   ```

2. **Type Unification**
   - Unify at the argument level, not the tuple level
   - Use proper error context for each argument
   - Handle type mismatches gracefully

3. **Error Handling**
   ```scala
   // CORRECT
   case FunctionCallArityMismatchError(expected, actual, expr) =>
     s"Record constructor expected $expected arguments but got $actual"

   // INCORRECT
   case TypeError(msg) =>
     s"Type error in record constructor: $msg"
   ```

## Error Handling

1. **Platform-Agnostic Errors**
   - Error messages should reference source code types
   - Never expose internal type representations in errors
   - Use `TyckResult` for consistent error reporting

2. **Error Context**
   - Include source code location
   - Provide clear explanation of type mismatch
   - Suggest possible fixes when applicable

## Testing

1. **Cross-Platform Tests**
   - Write tests that work on both JVM and JS
   - Use platform-agnostic assertions
   - Test both success and error cases

2. **Test Organization**
   ```scala
   class TypeCheckerTest extends FunSuite {
     // Group tests by functionality
     test("record field access - direct") { ... }
     test("record field access - lazy reduction") { ... }
     test("type-level functions") { ... }
   }
   ```

3. **Common Test Issues**
   - Using platform-specific imports
   - Assuming specific type implementations
   - Not handling both platforms
   - Direct manipulation of internal types
