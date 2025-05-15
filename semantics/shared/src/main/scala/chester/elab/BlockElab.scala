package chester.elab

import chester.cell.CellEffects
import chester.error.Reporter
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.{Context, ContextItem, TyAndVal, convertMeta}
import chester.tyck.Tycker.MutableContext
import chester.uniqid.Uniqid
import chester.utils.elab.*

case object BlockElab extends Kind {
  type Of = BlockElab
}

case class BlockElab(block: Block, ty: CellRWOr[Term])(using effects: CellEffects, elab: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(BlockElab)
    with ConstraintTerm {
  override val result: CellRW[Term] = newHole
  given context: Context = ctx
  given Elab = elab
  given CellEffects = effects
}

case object BlockElabHandler extends Handler[ElabOps, BlockElab.type](BlockElab) {
  override def run(c: BlockElab)(using elab: ElabOps, solver: SolverOps): Result = {
    import c.{*, given}
    val exprStatements = block.statements.map(resolve(_))
    val outerContext = c.context
    given context: MutableContext = new MutableContext(outerContext)
    var statements: Vector[StmtTerm] = Vector()
    for (s <- exprStatements)
      s match {
        case let: LetDefStmt if let.kind == LetDefType.Let =>
          val pattern = let.defined
          val body = let.body.getOrElse { Reporter.report(???); ??? }
          pattern match {
            case DefinedPattern(pattern) => {
              val ty = toTerm(let.ty match {
                case Some(ty) => given_Elab.inferType(ty).wellTyped
                case _ => newType
              })
              val wellTyped = toTerm(given_Elab.check(body, ty))
              pattern match {
                case PatternBind(name, meta) => {
                  val id = Uniqid.generate[LocalV]
                  val localv = LocalV(name.name, ty, id, convertMeta(meta))
                  val r = elab.collector.newSymbol(localv, id, let, context.ctx)
                  context.update(_.add(ContextItem(name.name, id, localv, ty, Some(r))).knownAdd(id, TyAndVal(ty, wellTyped)))
                  statements = statements :+ LetStmtTerm(localv, wellTyped, ty, convertMeta(let.meta))
                }
                case _ => ???
              }
            }
            case _ => ???
          }
        case _ => ???
      }
    val resultExpr = block.result.getOrElse(UnitExpr(meta = None))
    val returning = toTerm(given_Elab.check(resultExpr, ty))
    // TODO: checking for possible leakage and do substitution for examples like {let a = Int; 1 : a}
    result.fill(BlockTerm(statements, returning, convertMeta(block.meta)))
    Result.Done
  }
}
