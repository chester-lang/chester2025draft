# Style Guide

## Core Principles

1. Use C-style Braces
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

2. No Indentation-Based Syntax
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

3. Function Definitions
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

4. Pattern Matching
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

5. For Comprehensions
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

## Additional Guidelines

- Use parentheses for method calls even when they could be omitted
- Prefer multi-line formatting with braces for complex expressions
- Use explicit type annotations for public APIs
- Keep line length reasonable (max 120 characters)
- Use two-space indentation within braces
