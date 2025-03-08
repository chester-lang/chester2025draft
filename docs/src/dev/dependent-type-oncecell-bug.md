# Dependent Type System: OnceCell Bug & Improvements

## Bug Description

When attempting to use a function at the type level in Chester's dependent type system, we encountered an `IllegalArgumentException` with the message "requirement failed". This occurred in the `OnceCell.fill` method at `ProvideCellId.scala:77`.

### Reproduction

The bug can be reproduced using the following Chester code:

```chester
// Define a simple type-level function
def TypeId(T: Type): Type = T;

// Use the type-level function
let c: TypeId(Integer) = 10;
```

### Stack Trace

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

## Problem Statement

The `OnceCell.fill` method is being called multiple times for the same cell during type checking of dependent types. Specifically, in `UnifyFunctionCall.run`, the `functionCallTerm` cell is filled twice, resulting in an `IllegalArgumentException` with the message "requirement failed".

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

## Investigation Findings

1. The issue occurs when processing function calls in the type checker, particularly for type-level functions.
2. The error happens in `OnceCell.fill` at line 85 in `ProvideCellId.scala` where there's a requirement that `value.isEmpty`.
3. Debug logs show that `UnifyFunctionCall.run` attempts to fill the same cell twice with the same value.
4. This behavior is likely due to the reducer not properly identifying already processed cells.

## Fix Options

We have two viable approaches:

### Option 1: Update OnceCell to Handle Idempotent Operations

Modify `OnceCell.fill` to check if the new value is equal to the existing value before throwing an exception. This would make filling a cell with the same value multiple times an idempotent operation.

### Option 2: Fix UnifyFunctionCall to Prevent Double Filling

Modify `UnifyFunctionCall.run` to check if the cell already has a value before attempting to fill it, preventing the redundant fill operation.

After analysis, **Option 2** appears to be the better solution as it addresses the root cause rather than modifying the core propagator behavior.

## Implementation Plan for Bug Fix

1. Add debug assertions to better understand the flow in `UnifyFunctionCall.run`
2. Modify `UnifyFunctionCall.run` to check if `functionCallTerm` already has a value
3. If it has a value, compare with the value we're trying to set
   - If equal, skip the fill operation
   - If different, log a warning (this would be an unexpected situation)
4. Create a test case that specifically tests the fix
5. Run the full test suite to ensure no regressions

### Code Changes in UnifyFunctionCall.run

```scala
// Construct the function call term with adjusted callings
val fCallTerm = FCallTerm(functionTerm, adjustedCallings, meta = None)
Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Created function call term: $fCallTerm")
Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: About to fill cell: $functionCallTerm")

// Check if the cell already has a value before attempting to fill it
// This prevents the "requirement failed" exception when OnceCell.fill is called twice
val existingValue = state.readUnstable(functionCallTerm)
if (existingValue.isEmpty) {
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Cell is empty, filling with: $fCallTerm")
  state.fill(functionCallTerm, fCallTerm)
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Successfully filled function call term")
} else {
  // The cell already has a value, check if it's the same value
  Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Cell already has value: ${existingValue.get}")
  if (existingValue.get == fCallTerm) {
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Values are equal, skipping redundant fill")
  } else {
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: WARNING: Attempted to fill cell with different value")
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: Existing: ${existingValue.get}")
    Debug.debugPrint(DebugCategory.Tyck, s"UnifyFunctionCall.run: New: $fCallTerm")
  }
}
```

## Debug Utility Improvement Plan

To better diagnose and prevent similar issues in the future, we should also improve our debugging capabilities. The current debug utility in Chester uses string literals as category identifiers, which is error-prone and doesn't leverage Scala's type system.

### Step 1: Define a Proper Enum for Debug Categories

- Create `DebugCategory` enum in the `Debug` object
- Replace boolean flags with a Set of enabled categories
- Add proper enable/disable methods

```scala
enum DebugCategory {
  case Cell     // For cell filling debug info
  case Tyck     // For type checker debug info
  case Reducer  // For reducer debug info
}

// Replace individual boolean flags
var enabledCategories: Set[DebugCategory] = Set.empty
```

### Step 2: Update Debug Methods to Use the Enum

- Modify `debugPrint` and `printCallStack` to accept enum values instead of strings
- Update the category checking logic to use the enum

```scala
def debugPrint(category: DebugCategory, message: => String): Unit = {
  if (isEnabled(category)) {
    println(s"[DEBUG:${category}] $message")
  }
}
```

### Step 3: Update All Debug Call Sites

- Update all existing calls to `debugPrint` and `printCallStack` to use enum values
- Files to update:
  - `ProvideCellId.scala`: Update OnceCell.fill method
  - `ElaboraterFunctionCall.scala`: Update UnifyFunctionCall.run method
  - `Reducer.scala`: Update reduceTypeStructure method
  - Any other files using the Debug utility

### Step 4: Update Tests

- Update `DebugTyckTest.scala` to use the new enum-based API
- Use `Debug.enable(DebugCategory.Cell)` instead of setting boolean flags

## Success Criteria

1. The test suite passes without any exceptions
2. The specific test case for dependent types passes
3. No double-filling of cells occurs during type checking
4. Debug output provides clear information about type checking and cell operations

## Test Strategy

1. Run existing tests to establish a baseline
2. Create a focused test case for the specific issue
3. Run the full test suite after implementing the fix
4. Test the debug output with the modified API
5. Run the `DebugTyckTest` test to verify debug messages appear correctly with the enum-based approach

## Expected Benefits

1. **Fixed Dependent Type Handling**: Properly working type-level function applications
2. **Type Safety in Debugging**: Compile-time checking of debug categories
3. **Improved Developer Experience**: IDE autocompletion for category values
4. **Better Maintainability**: Centralized definition of categories
5. **Readability**: Clear, intention-revealing code
6. **Extensibility**: Easier to add new debug categories in the future 