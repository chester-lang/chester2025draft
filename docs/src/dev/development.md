# Development Memo

## Development Practices

### Planning Changes

1. **Document Before Implementing**
   - Always document the steps you plan to take BEFORE making any code changes
   - Break down complex changes into clearly defined smaller steps
   - For each step, explain:
     - What will be changed
     - Why the change is needed
     - How the change relates to the larger goal
     - What tests will verify the change
   - Review your plan for completeness before starting implementation
   - Document any deviations from the plan that occur during implementation

2. **Use Step-by-Step Implementation**
   - After documenting your plan, implement one step at a time
   - Run the full test suite (`sbt rootJVM/test`) after each step
   - Commit logical units of work with clear messages
   - Do not proceed to the next step until the current step passes all tests

### Making Changes

1. **Keep Changes Small and Focused**
   - Make one logical change at a time
   - Break down large changes into smaller, independent steps
   - Each change should be easily reviewable and testable

2. **Testing Requirements**
   - **ALWAYS use the following commands for running tests:**
     ```bash
     # Run all tests
     sbt rootJVM/test

     # Run a specific test class
     sbt "rootJVM/testOnly chester.tyck.FilesTyckTest"
     ```
   - **DO NOT** use other project paths like `cli/test`, `semantic/test`, etc. as these may not execute tests correctly
   - **DO NOT** run tests from subdirectories - always run from the root project directory
   - **NEVER** use commands like `cd reader && sbt test` as this will not work correctly
   - ⚠️ **CRITICAL: NEVER use the `-z` test filter option** ⚠️
     - This option is broken and produces unreliable results
     - Tests may appear to pass when they actually fail
     - This can lead to false confidence in your changes
   - ⚠️ **CRITICAL: NEVER use `--` to pass arguments to tests** ⚠️
     - ⚠️ **ABSOLUTELY PROHIBITED**: `sbt "rootJVM/testOnly -- -t MyTest"`
     - ⚠️ **ABSOLUTELY PROHIBITED**: `sbt "rootJVM/testOnly -- -only file.chester"`
     - ⚠️ **DANGEROUS**: Using `--` with ANY arguments will cause tests to run incorrectly
     - ⚠️ **DO NOT ATTEMPT**: This is strictly forbidden and will produce incorrect results
     - There is NO VALID USE CASE for passing arguments after `--` to any test
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
   - [ ] **CRITICAL: Always review the git diff output carefully**
     ```bash
     # Before committing, always check:
     git diff | cat
     ```
   - [ ] **IMPORTANT**: Reviewing git diff output is essential for catching:
     - Accidental deletions of important methods or logic
     - Unintended modification of critical code
     - Formatting changes that might impact behavior
     - Changes to files you didn't intend to modify
   - [ ] Pay special attention to large diffs that might hide important changes
   - [ ] Verify no unrelated changes were included
   - [ ] When making multiple changes, review each file's diff separately for clarity

5. **Post-Commit Verification**
   - Always verify your changes after committing with `git diff HEAD^ HEAD | cat`
   - Check the diff output carefully to ensure:
     - No unintended changes were included
     - All intended changes were properly committed
     - File renames and deletions are correctly reflected
     - No sensitive or debug code was accidentally committed
     - No accidental deletions of important logic
   - Verify the commit message accurately describes the changes
   - For complex changes involving multiple files, check each file's changes individually

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

### Troubleshooting Development Issues

1. **Recovering from Broken Edit Tools**
   - If edit tools in your IDE or development environment are broken/malfunctioning, you can use git to recover:
     ```bash
     # Discard changes to a specific file
     git checkout -- path/to/file | cat

     # Discard all changes in the working directory
     git checkout -- . | cat

     # Revert to a specific commit
     git checkout [commit-hash] -- path/to/file | cat
     ```
   - This approach is especially useful when tools that normally handle editing break unexpectedly
   - Always verify what you're checking out before executing the command to avoid losing important changes

### AI Agent Testing Instructions

1. **Terminal Interruption Issues**
   - If you are an AI agent working on Chester code and notice:
     - Frequent `^C` characters appearing in command output
     - Commands being interrupted prematurely
     - Test results not displaying properly
     - Terminal output being cut off
   - STOP attempting to run tests and:
     - Inform the user about the terminal connection issues
     - Ask the user to run the tests manually
     - Request that the user provide the test results
     - This indicates a problem with the terminal connection, not with the code itself

2. **Test Running Best Practices for AI Agents**
   - **ALWAYS use these exact commands for running tests:**
     ```bash
     # Run all tests
     sbt rootJVM/test | cat

     # Run a specific test class (include quotation marks)
     sbt "rootJVM/testOnly chester.tyck.FilesTyckTest" | cat
     ```
   - **NEVER** attempt to run tests with other project paths like `cli/test`, `semantic/test`, etc.
   - ⚠️ **CRITICAL: NEVER use the `-z` test filter option** ⚠️
     - Example of what NOT to do: `sbt "rootJVM/testOnly chester.tyck.FilesTyckTest -z myTest"`
     - The `-z` flag is completely broken and will cause misleading results
     - Tests might appear to pass when they should fail
     - Using `-z` will lead to incorrect conclusions about code behavior
   - ⚠️ **CRITICAL: NEVER use `--` to pass arguments to tests** ⚠️
     - Example of what NOT to do: `sbt "rootJVM/testOnly -- -t MyTest"`
     - This will cause tests to run incorrectly or not at all
     - No arguments should be passed after the test class name
   - Always run full test suites rather than individual tests when possible
   - Verify that terminal commands execute completely before proceeding
   - If a test command produces an error about not finding the test class:
     - First try the full `rootJVM/test` command to run all tests
     - Then check if the test class path is correct
     - Do not experiment with different project paths
   - If tests are taking too long to complete, inform the user and suggest they run the tests locally

## Platform-Specific Type System Implementation

Chester implements its type system with a unified Term definition in `Term.scala`.

### Import Guidelines

1. **DO** use `import chester.syntax.core.*`
   - This will give you access to all the Term implementations

### Example

```scala
// CORRECT
import chester.syntax.core.*

// INCORRECT
import chester.syntax.core.spec.*
import chester.syntax.core.simple.{BlockTerm, FCallTerm}
```

### Pattern Matching and Type Usage

1. Use concrete types for pattern matching:
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

## Core Term Architecture and Cross-Platform Implementation

Chester uses a carefully designed architecture for its term representation to support multiple platforms:

### Core Files and Their Relationships

1. **Term.scala** (`syntax/shared/src/main/scala/chester/syntax/core/Term.scala`)
   - Contains all Term definitions
   - Shared across all platforms
   - Defines behavior, structure and implementation
   - Used by all platforms

The previous architecture with separate files (`spec/Term.scala`, `simple.scala`, and `truffle.scala`) has been refactored. Now all Term definitions are in a single `Term.scala` file.

### Platform-Specific Export Mechanism

The core term implementation is exported through a single unified Term.scala file.

### Making Changes to the Term System

When adding or modifying types in the term system, changes should be made directly in `Term.scala`:

1. Define the trait or case class in `Term.scala`
   - Follow the existing patterns for similar term types
   - Use appropriate annotations like `@child` and `@const` for fields
   - Implement the required methods like `descent` and `toDoc`

### Example: Adding a New Term Type

To add a new term type (e.g., `TraitType`), you would add it directly to `Term.scala`:

```scala
case class TraitType(
  @child var traitDef: TraitStmtTerm,
  @const meta: OptionTermMeta
) extends TypeTerm {
  override type ThisTree = TraitType
  override def toDoc(using PrettierOptions): Doc =
    Doc.text("TraitType(") <> traitDef.toDoc <> Doc.text(")")
  override def descent(f: Term => Term, g: TreeMap[Term]): Term =
    thisOr(copy(traitDef = g(traitDef)))
}
```

### IMPORTANT: Keeping Types Consistent

It is **crucial** that:

- All term types follow a consistent pattern
- Field annotations are applied correctly
- All required methods are implemented
- Serialization with ReadWriter is handled properly

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
  // Use type-level reduction only for checking field existence
  // Keep original term in result
  // ...
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

## Style Guide

### String Formatting and Internationalization

1. **Use Template Strings for User-Facing Text**
   - ALWAYS use template strings (`t""`) for user-facing messages, not string interpolation (`s""`)
   - ALWAYS use template strings (`t""`) for plain user-facing text, even without variables
   - Always import the internationalization package: `import chester.i18n.*`
   - This ensures proper internationalization and localization support
   ```scala
   // CORRECT - using template strings for user-facing text
   import chester.i18n.*
   
   val username = "Alice"
   val message = t"Hello $username, welcome to Chester!"
   
   // CORRECT - using template strings for plain text without variables
   val errorMessage = t"Operation failed. Please try again."
   
   // INCORRECT - using string interpolation for user-facing text
   val message = s"Hello $username, welcome to Chester!"
   
   // INCORRECT - using regular string literals for user-facing text
   val errorMessage = "Operation failed. Please try again."
   ```

2. **String Interpolation for Internal Use Only**
   - Only use string interpolation (`s""`) for internal, non-user-facing strings
   - Examples include debug logging, internal identifiers, and non-displayed text
   ```scala
   // CORRECT - using string interpolation for internal/technical content
   val logMessage = s"DEBUG: Processing request from $username with params $params"
   val technicalId = s"${prefix}_${uuid}"
   ```

3. **Why This Matters**
   - Template strings enable automatic translation and localization
   - They maintain consistent messaging across the application
   - They allow for future language additions without code changes
   - They ensure a better experience for non-English users

### Core Principles

1. **Use C-style Braces**
   - Always use braces for control structures, even for single-line blocks
   - Opening brace on the same line
   - Closing brace on its own line
   ```scala
   // Good
   if (condition) {
     doSomething()
   } else {
     doSomethingElse()
   }

   // Bad - No braces
   if (condition)
     doSomething()

   // Bad - Indentation-based syntax
   if (condition)
     doSomething()
     andThenThis()  // Unclear scope
   ```

2. **No Indentation-Based Syntax**
   - Do not rely on indentation for scope
   - Always use explicit braces to define scope
   ```scala
   // Good
   def method() = {
     val result = {
       val x = compute()
       transform(x)
     }
     result
   }

   // Bad - Indentation-based
   def method() =
     val result =
       val x = compute()
       transform(x)
     result
   ```

3. **Function Definitions**
   - Opening brace on the same line as the function definition
   - Use explicit return types
   ```scala
   // Good
   def process(input: String): Result = {
     // implementation
   }

   // Bad
   def process(input: String): Result =
     // implementation
   ```

4. **Pattern Matching**
   - Use braces for case blocks
   - Align case statements
   ```scala
   // Good
   expr match {
     case Literal(value) => {
       process(value)
     }
     case Identifier(name) => {
       lookup(name)
     }
   }

   // Bad
   expr match
     case Literal(value) =>
       process(value)
     case Identifier(name) =>
       lookup(name)
   ```

5. **For Comprehensions**
   - Use braces instead of indentation
   ```scala
   // Good
   for {
     x <- xs
     y <- ys
   } yield {
     combine(x, y)
   }

   // Bad
   for
     x <- xs
     y <- ys
   yield combine(x, y)
   ```

### Additional Guidelines

- Use parentheses for method calls even when they could be omitted
- Prefer multi-line formatting with braces for complex expressions
- Use explicit type annotations for public APIs
- Keep line length reasonable (max 120 characters)
- Use two-space indentation within braces

### Enum Usage

1. **Prefer Enums Over String Literals**
   - Use enums for representing categories, types, or states
   - Never use string literals as pseudo-enums
   ```scala
   // Good
   enum DebugCategory {
     case Cell
     case Tyck
     case Reducer
   }

   // Bad
   val category = "CELL" // Using string literals as enum values
   ```

2. **Enum Naming Conventions**
   - Use PascalCase for enum type names
   - Use PascalCase for enum values
   - Keep enum names clear and descriptive

3. **Enum Usage**
   - Import enum values when needed
   - Use qualified access for clarity in other contexts
   - Use pattern matching for exhaustive handling
   ```scala
   // Good usage
   import DebugCategory.*

   val category = Cell

   category match {
     case Cell => handleCell()
     case Tyck => handleTyck()
     case Reducer => handleReducer()
   }
   ```

## Debugging Practices

1. **Understand Before Fixing**
   - Always understand the root cause of an issue before attempting to fix it
   - Use the Debug utility with appropriate categories to trace program execution
   - Analyze call stacks to identify where issues occur
   - Create minimal test cases that reproduce the issue

2. **Systematic Debugging Process**
   - Enable relevant debug logging (`Debug.enable(DebugCategory.XXX)`)
   - Add strategic logging points to track object state and method execution
   - Verify assumptions about code behavior using logs and assertions
   - Isolate the issue by creating focused test cases
   - Document your findings to help others understand the problem

3. **Debug-First Approach**
   - When facing complex issues, prioritize debugging over immediate fixes
   - Add temporary debugging code when needed, but mark it clearly and remove when done
   - Consider adding permanent debugging hooks for areas prone to issues
   - Document debugging insights even if they seem obvious

### Debug Flags Management

Chester uses a centralized debug system that categorizes debug output by component type. This approach:
- Allows targeted debugging of specific components without noise from others
- Prevents debug flags scattered throughout the codebase
- Enables easy enabling/disabling of debug output

1. **Debug Category System**
   - All debug output is organized into categories defined in `Debug.scala`:
     ```scala
     enum DebugCategory {
       case Cell          // For cell filling debug info
       case Tyck          // For type checker debug info
       case Reducer       // For reducer debug info
       case UnionSubtyping// For union subtyping debug info
       case UnionMatching // For union matching debug info
       case Literals      // For literal type handling debug info
       case Identifiers   // For identifier handling debug info
       case MethodCalls   // For method call handling debug info
       case StringArgs    // For string args handling debug info
       case TraitMatching // For trait implementation matching
     }
     ```

2. **Using Debug Logging**
   - Enable categories explicitly with `Debug.enable(DebugCategory.XXX)`
   - Log with category specificity: `Debug.debugPrint(DebugCategory.Tyck, "message")`
   - Check if category enabled: `if (Debug.isEnabled(DebugCategory.Tyck)) { ... }`
   - Stack traces: `Debug.printCallStack(DebugCategory.Tyck)`

3. **Debug Flags Script**
   - Use `debug-flags.sh` to manage debug flags in your shell sessions:
     ```bash
     # First, source the script to load functions
     source ./debug-flags.sh

     # Enable specific debug categories
     enable_debug ENV_DEBUG_UNION_MATCHING

     # Enable all debug categories
     enable_all_debug

     # Disable all debug categories
     disable_all_debug

     # Show current debug flag status
     show_debug_status

     # List available debug flags
     list_debug_flags
     ```

4. **Environment Variables**
   - Any debug category can be enabled via environment variables
   - Global debug flag: `ENV_DEBUG=true`
   - Category-specific flags: `ENV_DEBUG_UNION_MATCHING=true`
   - One-time usage: `ENV_DEBUG=true sbt rootJVM/test`

5. **Best Practices for Debug Logging**
   - Debug flags are all disabled by default for better performance
   - Enable only the specific categories you need
   - Keep debug statements clear, concise, and helpful
   - Clean up any temporary debug code when done
   - Add new categories to `DebugCategory` enum when needed for new components
   - Remember to source the `debug-flags.sh` script when starting a new shell
