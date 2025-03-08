# Algebraic Effects System in Chester

## Overview

This document outlines the plan for implementing an algebraic effects system in Chester. The implementation will follow Chester's development practices, with a focus on small, incremental changes that can be tested at each step.

### What are Algebraic Effects?

Algebraic effects provide a structured way to handle side effects in a program. Unlike exceptions or monads, algebraic effects:

1. Allow for precise effect tracking in the type system
2. Enable local reasoning about effectful code
3. Support composing multiple effects
4. Provide a way to handle effects at different levels of the call stack

## Current Status

Chester's codebase already has initial infrastructure for effects:

1. `EffectsCell` trait in `ElaboraterCommon.scala` for tracking effects
2. Various effect-related code in the elaborator
3. Placeholder for the `requireEffect` method that needs implementation

## Implementation Plan

### Phase 1: Core Effect Checking

#### Step 1: Implement Error Types for Effects

- Add `CannotAddEffectError`, `UnauthorizedEffectError`, and `MissingEffectHandlerError` to `TyckProblem.scala`
- These will be used to report issues related to effect checking

#### Step 2: Implement `requireEffect` Method

- Complete the `requireEffect` method in `EffectsCell` trait
- Implement efficient checks for existing effects
- Add proper error reporting

#### Step 3: Enhance Function Type to Track Effects

- Update function type elaboration to properly handle effect annotations
- Ensure effect information is preserved during type checking

#### Step 4: Implement Effect Checking in Function Call Elaboration

- Verify that functions aren't called with effects they don't declare
- Propagate effects from function calls to their containing blocks

#### Step 5: Add Parser Support for Effect Syntax

- Extend the parser to handle effect declarations in function signatures
- Support syntax for both declaring and handling effects

#### Step 6: Update Test Cases

- Add test cases for effect checking
- Ensure existing tests continue to pass

### Phase 2: Effect Handlers

#### Step 1: Implement `handle` Expression

- Add support for `handle` expressions to handle specific effects
- Update the parser to recognize handler syntax
- Implement type checking for handlers

#### Step 2: Handler Type Checking

- Check that handlers provide implementations for all effects they claim to handle
- Verify that effects are properly tracked in the presence of handlers

#### Step 3: Test Cases for Handlers

- Add test cases for effect handlers
- Test interaction between multiple handlers

### Phase 3: Higher-Order Functions

#### Step 1: Effect Checking for Higher-Order Functions

- Extend effect checking to work with higher-order functions
- Handle effect polymorphism

#### Step 2: Update Test Cases for Higher-Order Functions

- Add test cases for higher-order functions with effects
- Test effect polymorphism

## Testing Strategy

For each implementation step:

1. Unit tests for the specific functionality
2. Integration tests with existing features
3. Full test suite (`sbt rootJVM/test`) to verify no regressions

## Success Criteria

The implementation will be considered successful when:

1. Programs can declare effects in function signatures
2. Type checking catches unauthorized effect usage
3. Effects can be handled through handler expressions
4. Higher-order functions work correctly with effects
5. All tests pass

## Implementation Notes

- Follow Chester's development practices as outlined in `development.md`
- Make small, incremental changes with thorough testing
- Document design decisions
- Ensure backward compatibility with existing code 