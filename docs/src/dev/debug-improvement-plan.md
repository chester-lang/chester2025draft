# Debug Utility Improvement Plan

## Problem Statement

The current debug utility in Chester uses string literals as category identifiers, which is error-prone and doesn't leverage Scala's type system. String literals can lead to bugs due to typos, lack of autocompletion support, and absence of compile-time checking.

## Proposed Changes

We will replace the string-based categories with a proper enum for better type safety and improved developer experience, following our style guidelines for enum usage.

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

## Expected Benefits

1. **Type Safety**: Compile-time checking of debug categories
2. **Improved Developer Experience**: IDE autocompletion for category values
3. **Better Maintainability**: Centralized definition of categories
4. **Readability**: Clear, intention-revealing code
5. **Extensibility**: Easier to add new debug categories in the future

## Testing Strategy

- Ensure all existing tests continue to pass after the changes
- Test the debug output with the modified API
- Run the `DebugTyckTest` test to verify debug messages appear correctly with the enum-based approach

## Implementation Notes

- Follow the proper enum usage guidelines from our development documentation
- Maintain backward compatibility where possible
- Apply changes incrementally, testing at each step 