# Development Memo

## Development Practices

### Making Changes

1. **Keep Changes Small and Focused**
   - Make one logical change at a time
   - Break down large changes into smaller, independent steps
   - Each change should be easily reviewable and testable

2. **Verify Changes with Git**
   ```bash
   # After each change:
   git diff | cat            # Review what changed (use | cat to avoid paging)
   git add <files>          # Stage specific files
   git status              # Verify staged changes
   git commit -m "..."     # Commit with clear message
   ```

3. **Change Verification Checklist**
   - [ ] Changes are minimal and focused
   - [ ] Git diff shows only intended changes
   - [ ] Tests pass after changes
   - [ ] Changes align with existing code style

4. **Git Command Tips**
   - Always use `| cat` with git commands that might trigger paging:
     ```bash
     git diff | cat
     git log | cat
     git show | cat
     ```
   - This ensures consistent output and avoids interactive paging

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

## Elaboration and Reduction Strategy

### Reduction During Type Checking

1. **Keep Original Forms**
   - The elaborator should preserve original terms in the elaborated result whenever possible
   - Reduced forms should only be used internally during type checking
   - This makes the elaborated code closer to source code, making it easier to:
     - Debug and understand the code
     - Apply further transformations
     - Generate better error messages

2. **When to Reduce**
   - Only reduce terms when needed for type checking, such as:
     - Checking field access on type-level functions
     - Resolving type equality
     - Evaluating type-level computations
   - Use `ReduceMode.TypeLevel` for type-level computations
   - Use `ReduceMode.Normal` only when full reduction is required

Example:
```scala
// Original code
def idType(x: Type): Type = x;
let aT = idType(A);
def getA(x: aT): Integer = x.a;

// During type checking: reduce idType(A) to check field access
// But in elaborated result: preserve idType(A) in the type annotation
```

### Reduction Context and Type Checking

1. **Reduction Context Setup**
   - Each `Context` instance provides its own reduction context via `toReduceContext`
   - The default reducer is `NaiveReducer`, provided as a given instance in `Context`
   - During type checking, use the context's reduction capabilities:
     ```scala
     given ReduceContext = localCtx.toReduceContext
     val reducedTerm = NaiveReducer.reduce(term, ReduceMode.TypeLevel)
     ```

2. **When to Use Reduction Context**
   - For type equality checking
   - When evaluating type-level expressions
   - During field access resolution on complex types
   - Any time type-level computation is needed

Example:
```scala
// When checking field access on a type:
given ReduceContext = localCtx.toReduceContext
val reducedRecordTy = NaiveReducer.reduce(recordTy, ReduceMode.TypeLevel)
reducedRecordTy match {
  case RecordType(fields, _) => // Work with the reduced type
  case _ => // Handle non-record types
}
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

## Type Checking Implementation Thinking

### Core Principles

1. **Separation of Concerns**
   - Keep elaboration separate from type checking
   - Maintain clear boundaries between phases
   - Use appropriate reduction modes for each phase

2. **Source Code Preservation**
   - Preserve original terms whenever possible
   - Only transform code when necessary for type checking
   - Maintain source code structure for better debugging

3. **Controlled Reduction**
   - Use reduction modes to control evaluation
   - Only reduce terms when needed for type checking
   - Keep original forms in elaborated results

### Common Implementation Pitfalls

1. **Premature Reduction**
   ```scala
   // WRONG: Don't reduce during elaboration
   def processLetLetDefStmt(...) = {
     val wellTyped = NaiveReducer.reduce(elab(expr.body.get))
     // ...
   }

   // RIGHT: Keep original terms
   def processLetLetDefStmt(...) = {
     val wellTyped = elab(expr.body.get)
     // ...
   }
   ```

2. **Incorrect Context Usage**
   ```scala
   // WRONG: Creating new context for each reduction
   def reduce(term: Term): Term = {
     val ctx = Context.default // Don't create new context
     // ...
   }

   // RIGHT: Use passed context
   def reduce(term: Term)(using ctx: ReduceContext): Term = {
     // ...
   }
   ```

3. **Type Checking vs. Elaboration Confusion**
   ```scala
   // WRONG: Mixing concerns
   def elab(expr: Expr): Term = {
     // Don't do type checking here
     val reduced = NaiveReducer.reduce(term)
     checkType(reduced)
     // ...
   }

   // RIGHT: Separate concerns
   def elab(expr: Expr): Term = {
     // Only elaborate, preserve structure
     // Type checking happens separately
   }
   ```

### Best Practices

1. **Error Handling**
   - Report errors at appropriate phase
   - Maintain source code locations
   - Provide clear error messages
   ```scala
   // Good error reporting
   case _ => 
     ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
   ```

2. **Context Management**
   - Pass context explicitly using `given`
   - Maintain context hierarchy
   - Use appropriate context for each phase
   ```scala
   def check(using ctx: Context, ck: Tyck) = {
     // Use passed context
   }
   ```

3. **Type-Level Computation**
   - Use type-level reduction only when needed
   - Preserve original type terms
   - Handle type equality carefully
   ```scala
   // Only reduce for type checking
   def unify(lhs: Term, rhs: Term) = {
     given ReduceContext = localCtx.toReduceContext
     val lhsReduced = NaiveReducer.reduce(lhs, ReduceMode.TypeLevel)
     // ...
   }
   ```

### Implementation Guidelines

1. **Phase Separation**
   - Elaboration: Transform source to core terms
   - Type Checking: Verify types and reduce when needed
   - Reduction: Control evaluation with modes

2. **Context Flow**
   - Pass context through using `given`
   - Update context appropriately
   - Maintain context hierarchy

3. **Error Management**
   - Report errors at right phase
   - Preserve source locations
   - Clear error messages