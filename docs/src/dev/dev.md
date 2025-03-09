# Development Quick Reference

## Development Practices

### Planning & Implementation
- Document changes BEFORE coding
- Break complex changes into smaller steps
- Run `sbt rootJVM/test` after each step
- Make small, focused, logical commits

### Testing Requirements
- Run tests before committing
- Add tests for new functionality
- **Parser Tests**:
  - Use `parseAndCheckBoth` for new tests (tests both V1 and V2)
  - Use `parseAndCheck` only for V1-specific features
  - Document V1-only tests with rationale

### Change Verification
- Review changes with `git diff | cat` before committing
- Check for:
  - Accidental deletions or modifications
  - Unintended changes
  - Tests passing
  - Code style alignment
- Verify after committing with `git diff HEAD^ HEAD | cat`

## Terminal Control
- **Always add `| cat` to Git commands** to prevent paging issues
- Examples:
  ```bash
  git diff | cat
  git log | cat
  git checkout main | cat
  git merge --no-ff branch | cat
  ```
- Prevents terminal from entering interactive mode

## Cross-Platform Type System

### Import Guidelines
1. ✅ Use `import chester.syntax.core.*`
2. ❌ DON'T import from `chester.syntax.core.spec.*`
3. ❌ DON'T import directly from platform-specific implementations

### Pattern Matching
```scala
// CORRECT
case t: BlockTerm => {
  val reducedStatements = t.statements.map(stmt => r.reduce(stmt))
  BlockTerm(reducedStatements, reducedResult, t.meta)
}

// INCORRECT - Don't use suffix traits
case t: BlockTermC[Term] => { ... }
```

## Elaboration & Reduction Strategy

### Key Principles
- **Keep Original Forms** - Never reduce during elaboration
- Use reduction ONLY for:
  1. Type equality checking in unification
  2. Field access checking on type-level terms
- Use `ReduceMode.TypeLevel` for internal reductions

### Common Pitfalls
- Over-reduction
- Loss of original terms
- Incorrect reduction context

## Style Guide

### Core Principles
- Use C-style braces (not indentation-based syntax)
- Opening brace on same line, closing on own line
- Always use braces for all control structures
- Use explicit return types

### Enum Usage
- Use enums for representing categories/states (not string literals)
- Use PascalCase for enum type names and values

## Debugging Practices
- Understand root cause before fixing
- Use Debug utility with appropriate categories
- Add strategic logging points
- Document debugging insights

---

*Note: This is a quick reference. See `development.md` for the complete detailed guidelines.* 