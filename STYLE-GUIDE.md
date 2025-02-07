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

## Type System and Cross-Platform Guidelines

1. Imports and Dependencies
```scala
// Good
import chester.syntax.core.*
import chester.error.*

// Bad - Platform-specific imports
import chester.syntax.core.truffle.*
import chester.syntax.core.simple.*
```

2. Type Declarations and Pattern Matching
```scala
// Good - Use concrete types without suffixes
case t: BlockTerm => {
  val statements = t.statements.map(reduce)
  BlockTerm(statements, t.result, t.meta)
}

// Bad - Using platform-specific traits
case t: BlockTermC[Term] => {
  val statements = t.statements.map { 
    case s: StmtTermT[Term] => reduce(s)
  }
}
```

3. Type Checking and Reduction
```scala
// Good - Lazy reduction
def checkField(record: Term): Type = {
  record match {
    case r: RecordTerm => r.fieldType
    case _ => Reducer.reduce(record) match {
      case r: RecordTerm => r.fieldType
      case _ => throw TypeError("Expected record")
    }
  }
}

// Bad - Eager reduction
def checkField(record: Term): Type = {
  val reduced = Reducer.reduce(record)  // Don't reduce unnecessarily
  reduced.fieldType
}
```

4. Error Handling
```scala
// Good - Platform-agnostic errors
case class TypeError(msg: String) extends Error {
  def format: String = s"Type error: $msg"
}

// Bad - Platform-specific error types
case class JVMTypeError(throwable: java.lang.Throwable)
```

5. Testing
```scala
// Good - Platform-independent test
test("record field access") {
  val record = Record("x" -> IntType)
  val result = typeCheck(record.x)
  assertEquals(result, IntType)
}

// Bad - Platform-specific test
test("JVM record implementation") {
  val record = JVMRecord(field("x", JVMIntType))
  assertTrue(record.isInstanceOf[JVMRecordImpl])
}
```

## Additional Guidelines

- Use parentheses for method calls even when they could be omitted
- Prefer multi-line formatting with braces for complex expressions
- Use explicit type annotations for public APIs
- Keep line length reasonable (max 120 characters)
- Use two-space indentation within braces

1. Documentation
- Document platform-specific behavior
- Explain type system implications
- Reference the development guide for complex topics

2. Error Messages
- Use source code types in error messages
- Keep error messages platform-agnostic
- Include context for type errors

3. Performance
- Use lazy evaluation when possible
- Cache computed types and reductions
- Consider both JVM and JS performance characteristics

For more detailed guidelines on type system implementation and cross-platform development, see the [Development Guide](docs/src/development.md).
