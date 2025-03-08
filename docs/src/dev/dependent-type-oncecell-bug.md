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

## Analysis

The root cause appears to be:

1. `OnceCell` is designed to only be filled once, with a requirement check: `require(value.isEmpty)`.
2. During the evaluation of type-level function applications like `TypeId(Integer)`, the type checking system tries to fill the same cell multiple times.
3. This happens in `UnifyFunctionCall.run` when processing function call terms at the type level.

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

1. Modifying the way type-level function applications are evaluated to avoid filling the same cell twice
2. Using a different cell type (like `MutableCell`) for type-level computations that might need multiple updates
3. Implementing proper handling of dependent types in the reducer and type checking integration

This issue is directly related to the integration between the `NaiveReducer` and type checking system for dependent types. 