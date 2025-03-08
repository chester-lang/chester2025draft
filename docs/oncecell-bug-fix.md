# OnceCell Bug Fix Plan

## Problem Statement

The `OnceCell.fill` method is being called multiple times for the same cell during type checking of dependent types. Specifically, in `UnifyFunctionCall.run`, the `functionCallTerm` cell is filled twice, resulting in an `IllegalArgumentException` with the message "requirement failed".

## Investigation Findings

1. The issue occurs when processing function calls in the type checker, particularly for type-level functions.
2. The error happens in `OnceCell.fill` at line 85 in `ProvideCellId.scala` where there's a requirement that `value.isEmpty`.
3. Debug logs show that `UnifyFunctionCall.run` attempts to fill the same cell twice with the same value.
4. This behavior is likely due to the reducer not properly identifying already processed cells.

## Proposed Fix

We have two viable approaches:

### Option 1: Update OnceCell to Handle Idempotent Operations

Modify `OnceCell.fill` to check if the new value is equal to the existing value before throwing an exception. This would make filling a cell with the same value multiple times an idempotent operation.

### Option 2: Fix UnifyFunctionCall to Prevent Double Filling

Modify `UnifyFunctionCall.run` to check if the cell already has a value before attempting to fill it, preventing the redundant fill operation.

After analysis, **Option 2** appears to be the better solution as it addresses the root cause rather than modifying the core propagator behavior.

## Implementation Plan

1. Add debug assertions to better understand the flow in `UnifyFunctionCall.run`
2. Modify `UnifyFunctionCall.run` to check if `functionCallTerm` already has a value
3. If it has a value, compare with the value we're trying to set
   - If equal, skip the fill operation
   - If different, log a warning (this would be an unexpected situation)
4. Create a test case that specifically tests the fix
5. Run the full test suite to ensure no regressions

## Success Criteria

1. The test suite passes without any exceptions
2. The specific test case for dependent types passes
3. No double-filling of cells occurs during type checking

## Test Strategy

1. Run existing tests to establish a baseline
2. Create a focused test case for the specific issue
3. Run the full test suite after implementing the fix 