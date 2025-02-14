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
   - The elaborator MUST preserve original terms in the elaborated result
   - NEVER reduce during elaboration
   - Only use reduction internally during type checking when absolutely necessary
   - This makes the elaborated code identical to source code, making it:
     - Easier to debug
     - Easier to understand
     - Better for error messages
     - More suitable for further transformations

2. **When to Reduce**
   - Only TWO places should use reduction:
     1. Type equality checking in unification
     2. Field access checking on type-level terms
   - Use `ReduceMode.TypeLevel` for these internal reductions
   - NEVER use reduction in elaborated results

Example:
```scala
// Original code
def idType(x: Type): Type = x;
let aT = idType(A);
def getA(x: aT): Integer = x.a;

// WRONG - reducing during elaboration:
LetStmtTerm(localv, reducer.reduce(idType(A)), ty, meta)

// RIGHT - keeping original term:
LetStmtTerm(localv, idType(A), ty, meta)

// RIGHT - internal reduction only for field checking:
def checkFieldAccess(recordTy: Term, field: Name): Term = {
  given ReduceContext = localCtx.toReduceContext
  val reducedTy = NaiveReducer.reduce(recordTy, ReduceMode.TypeLevel)
  // Check field exists, but keep original term in result
  ...
}
```

### Reduction Context and Type Checking

1. **Reduction Context Setup**
   - Each `Context` instance provides its own reduction context via `toReduceContext`
   - Only create reduction context when needed for type checking
   - NEVER create reduction context during elaboration

2. **When to Use Reduction Context**
   - ONLY for internal type checking operations:
     - Type equality checking
     - Field access verification
   - NEVER for elaboration or term construction

Example:
```scala
// WRONG - reducing during elaboration:
def processLetLetDefStmt(...) = {
  val reduced = NaiveReducer.reduce(expr)  // DON'T do this
  ...
}

// RIGHT - only reduce during type checking:
def unify(lhs: Term, rhs: Term) = {
  given ReduceContext = localCtx.toReduceContext
  val lhsReduced = NaiveReducer.reduce(lhs, ReduceMode.TypeLevel)
  val rhsReduced = NaiveReducer.reduce(rhs, ReduceMode.TypeLevel)
  // Compare reduced forms internally
}
```

### Implementation Guidelines

1. **Clear Phase Separation**
   - Elaboration: NEVER reduces, only transforms source to core terms
   - Type Checking: Uses reduction internally only when needed
   - Keep these phases strictly separate

2. **Term Preservation**
   - Always preserve original terms in elaborated results
   - Use reduction only for internal type checking operations
   - Never modify terms just for elaboration

3. **Minimal Reduction**
   - Only reduce when immediately necessary
   - Keep all reductions internal to type checker
   - Never reduce speculatively or "just in case"

4. **Error Handling**
   - Report errors using original, unreduced terms
   - Keep source locations and original structure
   - Make error messages match source code exactly

### Common Mistakes to Avoid

1. **Over-reduction**
   ```scala
   // WRONG:
   def elaborate(expr: Expr): Term = {
     NaiveReducer.reduce(transform(expr))  // Don't reduce during elaboration
   }

   // RIGHT:
   def elaborate(expr: Expr): Term = {
     transform(expr)  // Keep original term structure
   }
   ```

2. **Premature Reduction**
   ```scala
   // WRONG:
   def checkType(expr: Expr): Term = {
     val reduced = NaiveReducer.reduce(expr)  // Don't reduce before needed
     ...
   }

   // RIGHT:
   def checkType(expr: Expr): Term = {
     // Only reduce when specifically needed for type checking
     ...
   }
   ```

3. **Loss of Source Structure**
   ```scala
   // WRONG:
   def processLet(name: Name, value: Term): Term = {
     val reduced = NaiveReducer.reduce(value)  // Don't reduce let bindings
     LetStmtTerm(name, reduced, ...)
   }

   // RIGHT:
   def processLet(name: Name, value: Term): Term = {
     LetStmtTerm(name, value, ...)  // Keep original value
   }
   ```

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