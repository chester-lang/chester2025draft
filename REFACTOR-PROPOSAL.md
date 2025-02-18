# Elaborater Refactoring Proposal

## Current Issues
1. The codebase mixes trait definitions with their implementations in a way that's hard to maintain
2. The `Elaborater*` traits serve as interfaces while `Provide*` traits serve as implementations, but the separation is not clean
3. There are mutual dependencies between traits that make the current interface/implementation split ineffective
4. There's a lot of boilerplate and repeated code patterns

## Proposed Solution Using Scala 3 Features

### 1. Capability Traits with Self-Types
Instead of using inheritance for implementation, use self-types to declare capabilities:

```scala
// Core capabilities
trait ElaborationCapability {
  def newType: CellId[Term]
  def unify(lhs: Term, rhs: Term): Unit
}

trait TypeCheckingCapability {
  def checkType(expr: Expr): Term
  def elabTy(expr: Option[Expr]): Term
}

// Elaboration capabilities
trait BlockElaborater { this: ElaborationCapability & TypeCheckingCapability =>
  def elabBlock(
    expr: Block, 
    ty0: CellIdOr[Term], 
    effects: CIdOf[EffectsCell]
  ): BlockTerm
  
  protected def processDefLetDefStmt(
    expr: LetDefStmt,
    ctx: Context,
    declarationsMap: Map[Expr, DeclarationInfo],
    effects: CIdOf[EffectsCell]
  ): (Seq[StmtTerm], Context)

  protected def processRecordStmt(
    expr: RecordStmt,
    ctx: Context,
    declarationsMap: Map[Expr, DeclarationInfo],
    effects: CIdOf[EffectsCell]
  ): (Seq[StmtTerm], Context)
}

trait FunctionElaborater { this: ElaborationCapability & TypeCheckingCapability =>
  def elabFunction(
    expr: FunctionExpr, 
    ty0: CellIdOr[Term], 
    effects: CIdOf[EffectsCell]
  ): Term
}

trait TypeElaborater { this: ElaborationCapability =>
  def elab(
    expr: Expr, 
    ty: CellIdOr[Term], 
    effects: CIdOf[EffectsCell]
  ): Term
}
```

### 2. Use Intersection Types for Flexible Composition
Instead of rigid inheritance hierarchies, use intersection types to compose functionality:

```scala
type FullElaborater = BlockElaborater & FunctionElaborater & TypeElaborater

def createElaborater(using ctx: Context): FullElaborater = {
  new BlockElaborater with FunctionElaborater with TypeElaborater 
    with ElaborationCapability with TypeCheckingCapability {
    // Implementation details
  }
}
```

## Benefits
1. **Clear Capability Requirements**: Self-types make it explicit what capabilities each component needs
2. **Flexible Composition**: Intersection types allow for more flexible composition of features
3. **Better Encapsulation**: Implementation details stay in the implementation traits
4. **Reduced Coupling**: Components only depend on the capabilities they need
5. **Easier Testing**: Can mock individual capabilities for testing

## Migration Strategy
1. Define core capabilities as traits
2. Convert existing traits to use self-types for declaring dependencies
3. Use intersection types to compose the final implementation
4. Gradually migrate existing code to new structure

## Files to Modify
1. `ElaboraterBlock.scala`
2. `ElaboraterBase.scala`
3. `ElaboraterFunction.scala`
4. `ElaboraterFunctionCall.scala`
5. `ElaboraterCommon.scala`

## Next Steps
1. Review and discuss this revised proposal
2. Create proof of concept focusing on `ElaboraterBlock` and its dependencies
3. Test the new structure with existing functionality
4. Plan full migration if proof of concept is successful 