# Effect System Implementation Plan

## Goal

Enable type checking for the simplest effects example in `tests/tyck/effects-basic.chester.todo` by implementing the necessary compiler components for the built-in effects system.

## Current Status

- We have a basic effects syntax design specified in `docs/src/dev/effects-system.md`
- A simple test file `tests/tyck/effects-basic.chester.todo` exists but doesn't type check yet
- Some of the infrastructure for effects already exists in the codebase

## Implementation Tasks

### 1. Parser Enhancements

- Add parsing support for the `/` effect annotation syntax in function types
- Parse built-in effect names (`IO`, `State`, etc.) as identifiers
- Parse effect combinations using the `&` operator
- Generate AST nodes that correctly represent effects in function signatures

```scala
// Example parser implementation:
def functionDef(): FunctionDef = {
  // Parse function signature...
  
  // Check for effect annotation
  val effects = if (consume("/")) {
    parseEffects()
  } else {
    Effects.Empty
  }
  
  FunctionDef(name, params, returnType, effects, body)
}
```

### 2. Built-in Effects Registry

- Define a registry for built-in effect types
- Implement recognition of built-in effects like `IO` and `State`
- Add a mechanism to validate that effect identifiers refer to built-in effects

```scala
// Example registry:
object BuiltinEffects {
  val IO = "IO"
  val State = "State"
  val Exception = "Exception"
  val NonTermination = "NonTermination"
  
  val all = Set(IO, State, Exception, NonTermination)
  
  def isBuiltinEffect(name: String): Boolean = all.contains(name)
}
```

### 3. Type Representation

- Enhance the `Effects` class to handle built-in effects
- Implement function type representation that includes effects
- Support effect combinations (union of effects)

```scala
// Example Effects class:
case class Effects(effectMap: Map[String, Term]) {
  def combine(other: Effects): Effects = {
    Effects(this.effectMap ++ other.effectMap)
  }
  
  def contains(effect: String): Boolean = effectMap.contains(effect)
}
```

### 4. Type Checking

- Implement effect checking during function definition
- Verify that function bodies only use effects that are declared
- Track effect requirements in the function call chain
- Ensure effects are properly propagated from callee to caller

```scala
// Example type checker for function calls:
def checkFunctionCall(call: FunctionCall, context: Context): Type = {
  val funcType = checkExpr(call.function, context)
  
  // Check argument types...
  
  // Extract function effects
  val functionEffects = extractEffects(funcType)
  
  // Ensure the current context allows these effects
  checkEffectsAllowed(functionEffects, context.allowedEffects)
  
  // Return function's return type with effects
  ReturnType(funcType, functionEffects)
}
```

### 5. Effect Propagation

- Implement a mechanism to accumulate effects from function calls
- Ensure effects are propagated up the call chain
- Handle effect combinations correctly

```scala
// Example propagation:
def propagateEffects(callerEffects: Effects, calleeEffects: Effects): Effects = {
  // Combine effects from caller and callee
  callerEffects.combine(calleeEffects)
}
```

### 6. Error Reporting

- Add clear error messages for effect-related type errors
- Report when a function uses unauthorized effects
- Report when effect types are invalid or unknown

```scala
// Example error generation:
def unauthorizedEffectError(effect: String, context: Context): Error = {
  Error(s"Function uses effect '$effect' but it is not declared in the function signature", 
        context.location)
}
```

## Implementation Order

1. **First Phase: Basic Type Representation**
   - Enhance the AST and type system to represent effects
   - Implement the built-in effects registry
   - Add effect annotations to function types

2. **Second Phase: Parser Integration**
   - Update the parser to handle effect annotations
   - Parse effect combinations
   - Generate correct AST nodes with effects

3. **Third Phase: Type Checking**
   - Implement basic effect checking for function bodies
   - Add effect validation in function calls
   - Ensure effects are correctly propagated

4. **Fourth Phase: Error Handling**
   - Add descriptive error messages
   - Implement suggestions for fixing effect-related errors

5. **Fifth Phase: Testing**
   - Convert the `.todo` test to a regular test file
   - Add additional tests for more complex effect scenarios
   - Verify compatibility with existing code

## Timeline

- Days 1-2: First Phase implementation
- Days 3-4: Second Phase implementation
- Days 5-7: Third Phase implementation
- Days 8-9: Fourth Phase implementation
- Day 10: Testing and refinement

## Success Criteria

- The `tests/tyck/effects-basic.chester.todo` file can be renamed to `tests/tyck/effects-basic.chester` and passes type checking
- The type checker correctly enforces effect constraints
- Effect propagation works as expected in nested function calls
- Pure functions don't require effect annotations
- Clear error messages are provided for effect-related type errors 