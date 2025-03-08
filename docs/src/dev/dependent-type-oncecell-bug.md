# OnceCell Bug in Type-Level Function Application

## Bug Description

When attempting to use a function at the type level in Chester's dependent type system, we encountered an `IllegalArgumentException` with the message "requirement failed". This occurred in the `OnceCell.fill` method at `ProvideCellId.scala:77`.

## Reproduction

The bug can be reproduced using the following Chester code:

```chester
// Define a simple type-level function
def TypeId(T: Type): Type = T;

// Use the type-level function
let c: TypeId(Integer) = 10;
```

## Stack Trace

The error occurs in `OnceCell.fill` with the following stack trace:

```
java.lang.IllegalArgumentException: requirement failed
    at scala.Predef$.require(Predef.scala:324)
    at chester.utils.propagator.ProvideCellId$OnceCell.fill(ProvideCellId.scala:77)
    at chester.utils.propagator.ProvideCellId$OnceCell.fill(ProvideCellId.scala:76)
    at chester.utils.propagator.ProvideCellId.chester$utils$propagator$ProvideCellId$StateAbility$$_$fill$$anonfun$1(ProvideCellId.scala:161)
    at chester.utils.propagator.ProvideMutable$Impl.update(ProvideMutable.scala:57)
    at chester.utils.propagator.ProvideMutable$Impl.update(ProvideMutable.scala:52)
    at chester.utils.propagator.ProvideCellId$StateAbility.fill(ProvideCellId.scala:161)
    at chester.utils.propagator.ProvideCellId$StateAbility.fill$(ProvideCellId.scala:148)
    at chester.utils.propagator.ProvideMutable$Impl.fill(ProvideMutable.scala:42)
    at chester.tyck.ProvideElaboraterFunctionCall$UnifyFunctionCall.run(ElaboraterFunctionCall.scala:136)
    at chester.tyck.ProvideElaboraterFunctionCall$UnifyFunctionCall.run(ElaboraterFunctionCall.scala:126)
    ...
```

## Detailed Analysis

The root cause appears to be:

1. `OnceCell` is designed to only be filled once, with a requirement check: `require(value.isEmpty)`.
2. During the evaluation of type-level function applications like `TypeId(Integer)`, the type checking system tries to fill the same cell multiple times.

After examining the code, we've identified specific components where the bug is occurring:

### In UnifyFunctionCall.run (ElaboraterFunctionCall.scala:136)

```scala
// Construct the function call term with adjusted callings
val fCallTerm = FCallTerm(functionTerm, adjustedCallings, meta = None)
state.fill(functionCallTerm, fCallTerm)
```

When processing a type-level function application, this code:
- Creates a function call term
- Attempts to fill the `functionCallTerm` cell
- If this cell is an `OnceCell` and has already been filled earlier in the process, it fails

### In NaiveReducer (Reducer.scala)

The reducer has special handling for type-level function calls:

```scala
case fcall: FCallTerm if isTypeLevel(fcall) =>
  // First reduce normally
  val reduced = reduceStandard(fcall, ReduceMode.TypeLevel)
  // Then check if the result needs further type structure handling
  reduced match {
    // If still a complex type after reduction, process it recursively
    case Union(_, _) | Intersection(_, _) => reduceTypeStructure(reduced)
    case _ => reduced
  }
```

The interaction between the reducer and type checker during dependent type evaluation seems to be causing multiple processing of the same term, leading to attempts to fill the same cell twice.

## Exact Bug Location

The error occurs when:
1. The type checker processes a type-level function like `TypeId(Integer)`
2. During this processing, it creates a function call term and fills a cell using `state.fill(functionCallTerm, fCallTerm)`
3. Later in the process, possibly due to recursion or multiple phases of checking, it attempts to fill the same cell again
4. The `OnceCell.fill` method checks `require(value.isEmpty)` and fails when it finds the cell already has a value

## Workaround

Currently, type-level function applications cannot be used directly in type annotations. Value-level operations work correctly:

```chester
// This works fine
def Id(x) = x;

let a: Integer = 42;
let b: Integer = Id(a);

// This also works - using the function at value level
let c = Id(10);
```

## Potential Fixes

To address this issue, we should consider:

1. Modifying the `UnifyFunctionCall` implementation to check if a cell has already been filled before attempting to fill it again
2. Using `MutableCell` instead of `OnceCell` for cells that might be filled multiple times during dependent type processing
3. Implementing a more careful tracking mechanism for type-level function reductions to avoid redundant processing
4. Enhancing the reducer's type-level function handling to properly coordinate with the type checker

This issue is directly related to the integration between the `NaiveReducer` and type checking system for dependent types. Fixing it is crucial for proper support of dependent typing in Chester. 