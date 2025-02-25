# Development Memo

## Development Practices

### Making Changes

1. **Keep Changes Small and Focused**
   - Make one logical change at a time
   - Break down large changes into smaller, independent steps
   - Each change should be easily reviewable and testable

2. **Testing Requirements**
   - ALWAYS run `sbt rootJVM/test` before committing changes
   - Fix any test failures before committing
   - Add new tests for new functionality
   - Update existing tests when modifying behavior
   - Test both success and failure cases
   - For parser changes:
     - Many tests now run against both old and new readers (V1 and V2)
     - Some complex tests currently only run against V1 (original reader)
     - When adding new parser tests:
       - Use `parseAndCheckBoth` by default for new tests
       - Only use `parseAndCheck` if testing V1-specific features
       - Document if test is V1-only and why
       - Plan to migrate V1-only tests to V2 when ready
     - Test function usage:
       - `parseAndCheck`: V1 parser only
       - `parseAndCheckBoth`: Both V1 and V2 parsers
       - `parseAndCheckV1`: Deprecated alias for parseAndCheckBoth
     - Recently migrated tests:
       - Basic operator sequence tests
       - Pattern matching tests with uniform symbol treatment
       - Simple expression tests
       - Function call tests
       - Dot notation tests
       - Object tests
       - Tuple tests
       - Vararg tests
       - Floating-point number parsing tests
       - List tests with mixed types
     - Tests still needing migration:
       - Complex operator sequences (prefix, mixfix)
       - Telescope parsing
       - Error handling
       - Source position tracking
   - For type checking changes:
     - Test term preservation in elaborated results
     - Test type-level computation works correctly
     - Test error reporting is accurate
     - Test edge cases and corner cases

3. **Verify Changes with Git**
   ```bash
   # After each change:
   git diff | cat            # Review what changed (use | cat to avoid paging)
   git add <files>          # Stage specific files
   git status              # Verify staged changes
   git commit -m "..."     # Commit with clear message
   ```

4. **Change Verification Checklist**
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

### Terminal Control with Git Commands

1. **Always Use `| cat` Suffix**
   - Git commands that might trigger paging or interactive prompts should always end with `| cat`
   - This ensures consistent output and prevents terminal control issues
   - Examples:
     ```bash
     git checkout main | cat
     git merge --no-ff branch | cat
     git log | cat
     git diff | cat
     git show | cat
     git branch | cat
     ```

2. **Common Git Operations**
   ```bash
   # Switching branches
   git checkout main | cat
   git checkout -b new-branch | cat

   # Merging
   git merge --no-ff feature-branch | cat
   git merge --abort | cat  # If merge conflicts occur

   # Viewing changes
   git status | cat
   git log --oneline | cat
   git show HEAD | cat

   # Committing
   git add . | cat
   git commit -m "type: description" | cat
   ```

3. **Why This Matters**
   - Prevents terminal from entering interactive mode
   - Ensures consistent output formatting
   - Avoids getting stuck in pagers like `less`
   - Makes automation and scripting more reliable

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
   - This ensures consistent reduction behavior during type checking
   - Allows for future extensions to reduction context

2. **Type-Level Reduction**
   - Only reduce type-level terms when necessary for type checking
   - Keep original terms in elaborated results
   - Use `ReduceMode.TypeLevel` to control reduction behavior

3. **Field Access Checking**
   - Use type-level reduction to verify field existence
   - Keep original terms in field access expressions
   - Report errors using original terms for better error messages

### Common Pitfalls

1. **Over-reduction**
   - Don't reduce terms during elaboration
   - Don't reduce terms when adding to context
   - Only reduce when needed for type checking

2. **Loss of Original Terms**
   - Always preserve original terms in elaborated results
   - Don't reflect internal reductions in output
   - Keep source code structure intact

3. **Incorrect Reduction Context**
   - Always use proper reduction context from current context
   - Don't create new reduction contexts unnecessarily
   - Use consistent reduction mode for type checking