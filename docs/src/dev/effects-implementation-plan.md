# Algebraic Effects Implementation Plan

This document provides a step-by-step implementation plan for the algebraic effects system as described in `effects-system.md`. Each step is designed to be independently testable and maintain compatibility with the existing codebase.

## Phase 1: Core Effect Checking

### Step 1: Implement the `requireEffect` Method in `EffectsCell`

**Goal**: Complete the `requireEffect` method in `ElaboraterCommon.scala` that currently has a `???` implementation.

**Files to modify**:
- `semantic/shared/src/main/scala/chester/tyck/ElaboraterCommon.scala`

**Implementation details**:
```scala
trait EffectsCell extends Cell[Effects] {
  def requireEffect(
      effect: Term
  )(using ck: Tyck, state: StateAbility[Tyck]): LocalV = {
    // Check if this effect already exists in the cell
    val currentEffects = this.readUnstable.map(_.effects).getOrElse(Map.empty)
    
    // Try to find an existing entry for this effect
    val existingKey = currentEffects.find { case (_, existingEffect) => 
      // We'd need a proper equality check here based on effect types
      ck.unify(effect, existingEffect, false) 
    }.map(_._1)
    
    // Return existing key or create a new one
    existingKey.getOrElse {
      // Create a new key for this effect
      val newKey = LocalV.fresh("effect")
      
      // Add the effect to the cell
      this match {
        case cell: DynamicEffectsCell => 
          state.updateCell(cell.cid, cell.add(newKey, effect))
        case _ => 
          // For other cell types, we may need different handling
          ck.reporter(CannotAddEffectError(effect))
      }
      
      newKey
    }
  }
}
```

**Testing**:
- Create unit tests that add effects to a cell and verify they're stored correctly
- Create tests that try to add the same effect twice and verify it's only stored once
- Run the full test suite to ensure no regressions

### Step 2: Create Effect-Related Error Types

**Goal**: Define error types for effect checking failures in the error package.

**Files to modify**:
- `syntax/shared/src/main/scala/chester/error/TyckProblem.scala`

**Implementation details**:
```scala
// In chester.error package
case class CannotAddEffectError(effect: Term) extends TyckProblem {
  override def message: String = s"Cannot add effect ${effect.toDoc.renderString} to this context"
}

case class UnauthorizedEffectError(effect: Term, functionTerm: Term) extends TyckProblem {
  override def message: String = 
    s"Function ${functionTerm.toDoc.renderString} uses effect ${effect.toDoc.renderString} but doesn't declare it"
}

case class MissingEffectHandlerError(effect: Term) extends TyckProblem {
  override def message: String = 
    s"Effect ${effect.toDoc.renderString} is used but not handled in the current scope"
}
```

**Testing**:
- Create tests that trigger these errors and verify they're reported correctly
- Check that error messages are clear and helpful

### Step 3: Add Support for Effect Definition Syntax

**Goal**: Extend the parser to support the `effect` keyword and effect interface definitions.

**Files to modify**:
- `syntax/shared/src/main/scala/chester/syntax/concrete/Reader.scala` (or the new parser)
- Add AST nodes for effect definitions if needed

**Implementation details**:
```scala
// In the parser (pseudo-code, adapt to the actual parser implementation)
def parseTopLevel(): Expr = {
  if (token.text == "effect") {
    parseEffectDefinition()
  } else {
    // existing top-level parsers
    ...
  }
}

def parseEffectDefinition(): EffectDefExpr = {
  consume("effect")
  val name = parseIdentifier()
  val typeParams = if (peekToken().text == "[") {
    parseTypeParams()
  } else {
    Nil
  }
  
  consume("{")
  val operations = parseEffectOperations()
  consume("}")
  
  EffectDefExpr(name, typeParams, operations)
}

def parseEffectOperations(): Seq[EffectOperationExpr] = {
  val operations = mutable.ArrayBuffer[EffectOperationExpr]()
  
  while (token.text != "}") {
    operations += parseEffectOperation()
  }
  
  operations.toSeq
}

def parseEffectOperation(): EffectOperationExpr = {
  consume("def")
  val name = parseIdentifier()
  
  consume("(")
  val params = parseParams()
  consume(")")
  
  consume(":")
  val returnType = parseType()
  
  EffectOperationExpr(name, params, returnType)
}
```

**Testing**:
- Create tests with various effect definition syntaxes
- Verify that parsing preserves all effect information
- Run tests for both the original and new parsers if applicable

### Step 4: Add Support for Effect Annotations in Function Types

**Goal**: Extend the parser to support declaring effects in function types.

**Files to modify**:
- Parser implementation
- Type representation classes

**Implementation details**:
```scala
// In the parser (pseudo-code, adapt to the actual parser implementation)
def parseFunctionType(): Expr = {
  val paramName = parseIdentifier()
  consume(":")
  val paramType = parseType()
  consume("=>")
  val returnType = parseType()
  
  val effects = if (peekToken().text == "/") {
    consume("/")
    parseEffectsAnnotation()
  } else {
    // Empty effects for pure functions
    EmptyEffects()
  }
  
  FunctionTypeExpr(paramName, paramType, returnType, effects)
}

def parseEffectsAnnotation(): EffectsExpr = {
  if (token.text == "{") {
    // Multiple effects: {IO, Error}
    consume("{")
    val effects = parseCommaSeparatedList(() => parseType())
    consume("}")
    MultipleEffectsExpr(effects)
  } else {
    // Single effect: IO
    val effect = parseType()
    SingleEffectExpr(effect)
  }
}
```

**Testing**:
- Create tests for functions with various effect annotations
- Test both single effects and multiple effects
- Verify parsing and type representation are correct

### Step 5: Modify FunctionType to Include Effects

**Goal**: Update the function type representation to store effect information.

**Files to modify**:
- `syntax/shared/src/main/scala/chester/syntax/core/simple.scala` (or similar for your architecture)
- `syntax/shared/src/main/scala/chester/syntax/core/spec/Term.scala`

**Implementation details**:
```scala
// In the core representation
case class FunctionType(
  param: LocalV,
  paramTy: Term,
  returnTy: Term,
  effects: Term, // This will be an Effects instance
  meta: OptionTermMeta
) extends TypeTerm with FunctionTypeC[Term] {
  // Existing implementation...
  
  // Add methods to check if a specific effect is allowed
  def allowsEffect(effect: Term)(using ck: Tyck): Boolean = {
    effects match {
      case e: Effects => 
        e.effects.exists { case (_, declaredEffect) => 
          ck.unify(effect, declaredEffect, false)
        }
      case _ => false
    }
  }
}
```

**Testing**:
- Create tests that define functions with different effects
- Verify the type information is preserved correctly
- Test the `allowsEffect` method with various inputs

### Step 6: Update Function Call Checking to Verify Effects

**Goal**: Modify the function call elaboration to check and propagate effects.

**Files to modify**:
- `semantic/shared/src/main/scala/chester/tyck/ElaboraterFunctionCall.scala`

**Implementation details**:
```scala
// In the function call elaboration
def elaborateFunctionCall(
    f: Term, 
    args: Vector[Calling],
    meta: OptionTermMeta
)(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): (Term, Term, Effects) = {
  // Existing function call checking...
  
  // Check function's type
  val (fTerm, fType) = elaborate(f)
  
  // Extract function's declared effects
  val functionEffects = fType match {
    case ft: FunctionType => ft.effects
    case _ => Effects(Map.empty, None) // No effects if not a function type
  }
  
  // Collect effects from arguments
  val argEffects = args.flatMap { arg =>
    val (argTerm, argType, argEffects) = elaborate(arg.value)
    argEffects.asInstanceOf[Effects].effects
  }.toMap
  
  // Combine all effects
  val combinedEffects = Effects(
    functionEffects.asInstanceOf[Effects].effects ++ argEffects,
    None
  )
  
  // Return the elaborated function call with its type and effects
  (FCallTerm(fTerm, args, meta), returnType, combinedEffects)
}
```

**Testing**:
- Test function calls with effectful arguments
- Verify effects are properly propagated
- Test with both valid and invalid effect usages

### Step 7: Implement Effect Checking in Function Bodies

**Goal**: Verify that a function's body doesn't use effects not declared in its signature.

**Files to modify**:
- `semantic/shared/src/main/scala/chester/tyck/ElaboraterFunction.scala`

**Implementation details**:
```scala
// In the function elaboration
def elaborateFunction(
    param: LocalV,
    paramTy: Term,
    body: Term,
    declaredEffects: Term,
    meta: OptionTermMeta
)(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): (Term, Term) = {
  // Elaborate the body in a context with the parameter
  val bodyCtx = localCtx.addParam(param, paramTy)
  val (bodyTerm, bodyType, bodyEffects) = using(bodyCtx) { elaborate(body) }
  
  // Check that body effects are subset of declared effects
  val declaredEffectsMap = declaredEffects.asInstanceOf[Effects].effects
  
  for ((_, bodyEffect) <- bodyEffects.asInstanceOf[Effects].effects) {
    if (!declaredEffectsMap.exists { case (_, declaredEffect) => 
      ck.unify(bodyEffect, declaredEffect, false)
    }) {
      ck.reporter(UnauthorizedEffectError(bodyEffect, body))
    }
  }
  
  // Return the elaborated function with its type
  val functionType = FunctionType(param, paramTy, bodyType, declaredEffects, meta)
  (FunctionTerm(param, paramTy, bodyTerm, declaredEffects, meta), functionType)
}
```

**Testing**:
- Create tests with functions that use effects
- Include tests for correctly declared effects
- Include tests for effects that aren't declared but are used
- Verify error messages are helpful

### Step 8: Implement Basic Effect Operations

**Goal**: Add support for effect operations like `perform`.

**Files to modify**:
- Parser implementation
- Elaborator

**Implementation details**:
```scala
// In the parser (pseudo-code, adapt to the actual parser implementation)
def parsePerform(): PerformExpr = {
  consume("perform")
  val effect = parseReferenceCall()
  consume(".")
  val operation = parseIdentifier()
  
  consume("(")
  val args = parseArgs()
  consume(")")
  
  PerformExpr(effect, operation, args)
}

// In the elaborator
def elaboratePerform(
    effect: Term,
    operation: Name,
    args: Vector[Term],
    meta: OptionTermMeta
)(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): (Term, Term, Effects) = {
  // Look up the effect definition
  val effectDef = localCtx.lookupEffect(effect)
  
  // Find the operation in the effect
  val opDef = effectDef.operations.find(_.name == operation)
    .getOrElse(throw new RuntimeException(s"Operation $operation not found in effect $effect"))
  
  // Elaborate and check arguments
  val elaboratedArgs = args.zip(opDef.paramTypes).map { case (arg, paramTy) =>
    val (elaboratedArg, argTy) = elaborate(arg)
    ck.unify(argTy, paramTy)
    elaboratedArg
  }
  
  // Create effects
  val performEffects = Effects(Map(LocalV.fresh("effect") -> effect), None)
  
  // Return the elaborated perform with its type and effects
  (PerformTerm(effect, operation, elaboratedArgs, meta), opDef.returnType, performEffects)
}
```

**Testing**:
- Create tests for performing effects
- Test with various arguments and operations
- Verify that effects are properly tracked

## Phase 2: Effect Handlers (Future Work)

### Step 9: Add Support for Effect Handlers

**Goal**: Implement the syntax and semantics for handling effects.

**Files to modify**:
- Parser implementation
- Elaborator
- Runtime system (for actual effect handling)

**Implementation details**: (This is a sketch, details will be provided in a future plan)
```scala
// In the parser (pseudo-code)
def parseHandleWith(): HandleWithExpr = {
  consume("handle")
  val expr = parseExpr()
  consume("with")
  val handlers = parseHandlers()
  
  HandleWithExpr(expr, handlers)
}

def parseHandlers(): Seq[HandlerExpr] = {
  consume("{")
  val handlers = parseCommaSeparatedList(() => parseHandler())
  consume("}")
  
  handlers
}

def parseHandler(): HandlerExpr = {
  val effect = parseReferenceCall()
  consume(".")
  val operation = parseIdentifier()
  
  consume("(")
  val params = parseParams()
  consume(")")
  
  consume("=>")
  val body = parseExpr()
  
  HandlerExpr(effect, operation, params, body)
}
```

**Testing**:
- Create tests for handling effects
- Test resumption capabilities
- Verify effect elimination works correctly

## Phase 3: Higher-Order Functions (Future Work)

### Step 10: Implement Effect Polymorphism

**Goal**: Allow functions to be parameterized by effects.

**Implementation details**: (This is a sketch, details will be provided in a future plan)
```scala
// Example function with effect polymorphism
def runWith[E](f: () => Unit / E): Unit / E = {
  f()
}
```

## Verification Plan

After each step:

1. Run the full test suite: `sbt rootJVM/test`
2. Verify no regression in existing functionality
3. Check that new features work as expected
4. Review the changes with `git diff | cat`
5. Commit with a descriptive message
6. Verify the commit with `git diff HEAD^ HEAD | cat`

## Testing Plan

The following tests will be implemented to verify the effect system:

1. **Effect Declaration Tests**:
   - Test function type with single effect
   - Test function type with multiple effects
   - Test function type with no effects (pure)

2. **Effect Checking Tests**:
   - Test functions that correctly declare their effects
   - Test functions that use effects they don't declare (should fail)
   - Test effect propagation through function calls

3. **Effect Operation Tests**:
   - Test performing effect operations
   - Test effect operation with various arguments
   - Test that effect operations are properly tracked in types

4. **Handler Tests** (Future):
   - Test basic effect handlers
   - Test resumable computations
   - Test nested handlers
   - Test effect elimination 