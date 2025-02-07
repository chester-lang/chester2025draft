# Type System Architecture

## Overview

Chester's type system is designed to work consistently across JVM and JS platforms. This document explains the core architecture and implementation details.

## Core Components

### 1. Type Representation

```scala
// Core type interfaces
trait Type
trait Term
trait StmtTerm extends Term
trait RecordTerm extends StmtTerm
```

### 2. Platform-Specific Implementation

The type system is implemented differently for different platforms:

- JVM: Uses Truffle AST nodes
- JS: Uses simple class hierarchy

To maintain consistency:
1. Core types are defined in `chester.syntax.core`
2. Platform-specific implementations are in separate packages
3. The core package provides the correct implementation via exports

## Type Checking Process

### 1. Elaboration

The elaboration process converts concrete syntax into typed core terms:

```scala
def elab(expr: Expr, ty: Type, effects: Effects): Term = {
  expr match {
    case RecordExpr(fields) =>
      // Elaborate each field
      val elaboratedFields = fields.map(elaborateField)
      RecordTerm(elaboratedFields)
      
    case FunctionCall(func, args) =>
      // Handle function calls and record constructors
      elaborateFunctionCall(func, args)
  }
}
```

### 2. Record Type Handling

Records require special handling due to their dual nature as both types and terms:

1. **Record Definition**
   ```scala
   record Point {
     x: Integer
     y: Integer
   }
   ```
   This creates:
   - A record type `Point`
   - A constructor function `Point(x: Integer, y: Integer)`

2. **Record Construction**
   ```scala
   val p = Point(1, 2)
   ```
   This involves:
   - Type checking arguments against field types
   - Creating a record term
   - Verifying field access

### 3. Type Checking Strategy

The type checker uses a lazy reduction strategy:

1. **Direct Checking**
   - Try to type check without reduction first
   - Use concrete types when available

2. **Lazy Reduction**
   - Only reduce terms when necessary
   - Cache reduced forms for efficiency

3. **Type Propagation**
   ```scala
   case DotCall(record, field) =>
     val recordTy = newType
     val recordTerm = elab(record, recordTy)
     // Only reduce if needed
     val reduced = recordTerm match {
       case r: RecordTerm => r
       case _ => reducer.reduce(recordTerm)
     }
   ```

## Type Checking Architecture

### Components

1. **Tycker**
   - Main entry point for type checking
   - Manages type checking context and state
   - Handles error reporting

```scala
object Tycker extends DefaultImpl {
  def check(expr: Expr): TyckResult[Context, Judge] = {
    // Set up type checking context
    given Context = Context.empty
    given SemanticCollector = NoopSemanticCollector
    
    // Perform type checking
    val ty = newType
    val term = elab(expr, ty, effects)
    val judge = Judge(term, ty)
    
    // Return result with any errors
    TyckResult(context, judge, warnings, errors)
  }
}
```

2. **Judge**
   - Represents a type judgement
   - Contains both term and its type
   - Used for type checking results

```scala
case class Judge(term: Term, ty: Term)
```

3. **TyckResult**
   - Contains type checking result
   - Includes context, judgement, and any errors
   - Supports both success and failure cases

```scala
case class TyckResult[S, T](
  state: S,
  result: T,
  problems: Vector[TyckProblem]
)
```

### Type Checking Process

1. **Expression Analysis**
   ```scala
   def check(expr: Expr): TyckResult[Context, Judge] = {
     // 1. Create fresh type variable
     val ty = newType
     
     // 2. Elaborate expression
     val term = elab(expr, ty, effects)
     
     // 3. Create judgement
     val judge = Judge(term, ty)
     
     // 4. Return result
     TyckResult(context, judge, problems)
   }
   ```

2. **Error Handling**
   ```scala
   try {
     // Perform type checking
     val judge = check(expr)
     Success(judge)
   } catch {
     case e: TyckError =>
       Failure(Vector(e))
   }
   ```

3. **Context Management**
   ```scala
   given Context = Context.empty
   given SemanticCollector = NoopSemanticCollector
   given StateAbility[Tyck] = new StateAbility[Tyck] {
     def addPropagator(p: Propagator): Unit = p.propagate
   }
   ```

### Best Practices

1. **Type Variables**
   - Use `newType` for fresh type variables
   - Always propagate constraints
   - Handle unification carefully

2. **Error Reporting**
   - Report errors through the type checker
   - Include source locations
   - Provide helpful error messages

3. **Context Handling**
   - Keep context immutable
   - Use given instances for dependencies
   - Manage state carefully

## Type System Features

### 1. Record Types

Records in Chester have several important features:

1. **Field Types**
   - Static type checking for fields
   - Support for type-level computation

2. **Constructor Functions**
   - Automatically generated
   - Type-safe construction

3. **Field Access**
   - Lazy evaluation
   - Type preservation

### 2. Function Types

Functions support:

1. **Type Parameters**
   - Polymorphic functions
   - Type inference

2. **Effects**
   - Effect tracking
   - Effect polymorphism

### 3. Type-Level Computation

The type system supports:

1. **Type Functions**
   - Computation at the type level
   - Lazy evaluation

2. **Dependent Types**
   - Value-dependent types
   - Type-level arithmetic

## Implementation Guidelines

### 1. Type Checking

```scala
// CORRECT
def checkRecord(record: Term): Type = {
  record match {
    case r: RecordTerm => r.type
    case _ => 
      // Only reduce if needed
      reducer.reduce(record) match {
        case r: RecordTerm => r.type
        case _ => throw TypeError("Expected record")
      }
  }
}

// INCORRECT
def checkRecord(record: Term): Type = {
  // Don't always reduce
  val reduced = reducer.reduce(record)
  reduced.type
}
```

### 2. Error Handling

```scala
// CORRECT
case class TypeError(
  expected: Type,
  actual: Type,
  context: String
) extends Error {
  def message = s"Type mismatch: expected $expected but got $actual in $context"
}

// INCORRECT
case class PlatformTypeError(
  platform: String,
  error: java.lang.Exception
)
```

### 3. Testing

```scala
// CORRECT
test("record construction") {
  val record = Record("x" -> IntType)
  val result = typeCheck(record.x)
  assertEquals(result, IntType)
}

// INCORRECT
test("JVM record") {
  val record = JVMRecord(...)
  assertTrue(record.isInstanceOf[JVMRecordImpl])
}
