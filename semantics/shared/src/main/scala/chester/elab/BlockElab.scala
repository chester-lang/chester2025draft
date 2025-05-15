package chester.elab

import chester.cell.CellEffects
import chester.error.Reporter
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.{Context, convertMeta}
import chester.tyck.Tycker.MutableContext
import chester.utils.elab.*

case object BlockElab extends Kind {
  type Of = BlockElab
}

case class BlockElab(block: Block, ty: CellRWOr[Term])(using effects: CellEffects, elab: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(BlockElab)
    with ConstraintTerm {
  override def result: CellRW[BlockTerm] = newHole
  given context: Context = ctx
  given Elab = elab
  given CellEffects = effects
}

case object BlockElabHandler extends Handler[ElabOps, BlockElab.type](BlockElab) {
  override def run(c: BlockElab)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    val exprStatements = block.statements.map(resolve(_))
    val outerContext = c.context
    given context: MutableContext = new MutableContext(outerContext)
    var statements: Vector[StmtTerm] = Vector()
    for (s <- exprStatements)
      s match {
        case let: LetDefStmt if let.kind == LetDefType.Let =>
          val pattern = let.defined
          val body = let.body.getOrElse(Reporter.report(???))
          val ty = let.ty match {
            case Some(ty) => given_Elab.inferType(ty)
            case _        => ???
          }
        case _ => ???
      }
    val resultExpr = block.result.getOrElse(UnitExpr(meta = None))
    val returning = toTerm(given_Elab.elab(resultExpr, ty))
    // TODO: checking for possible leakage and do substitution for examples like {let a = Int; 1 : a}
    result.fill(BlockTerm(statements, returning, convertMeta(block.meta)))
    Result.Done
  }
}
