package chester.elab

import chester.elab.MutableContext
import chester.error.{MissingLetBody, Reporter}
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.{Context, ContextItem, TyAndVal, convertMeta}
import chester.resolve.ExprParser
import chester.uniqid.{Uniqid, UniqidOf}
import chester.utils.elab.*

import scala.collection.mutable

case object BlockElab extends Kind {
  type Of = BlockElab
}

case class BlockElab(block: Block, ty: CellRWOr[Term])(using elab: Elab, ops: SolverOps, ctx0: Context)
    extends Constraint(BlockElab)
    with ConstraintTerm {
  override val result: CellRW[Term] = newHole
  val context: Context = ctx0
  given Elab = elab
}

case object BlockElabHandler extends Handler[ElabOps, BlockElab.type](BlockElab) {
  override def run(c: BlockElab)(using elab: ElabOps, solver: SolverOps): Result = {
    import c.*
    val exprStatements = block.statements.map(resolve(_)(using c.context))
    // separate val and given definitions for better debugging in intellij idea
    val context: MutableContext = new MutableContext(c.context)
    given MutableContext = context
    var statements: Vector[StmtTerm] = Vector()
    val defs: mutable.Queue[(defined: Identifier, ty: CellRW[Term], id: UniqidOf[LocalV], localv: LocalV)] = mutable.Queue()
    exprStatements.foreach {
      case defstmt: LetDefStmt if defstmt.kind == LetDefType.Def =>
        defstmt.defined match {
          case _ @DefinedPattern(PatternBind(name, meta), _) =>
            val ty = newType
            val id = Uniqid.generate[LocalV]
            val localv = LocalV(name.name, toTerm(ty), id, convertMeta(meta))
            val r = elab.collector.newSymbol(localv, id, defstmt, context.ctx)
            context.update(_.add(ContextItem(name.name, id, localv, toTerm(ty), Some(r))))
            defs.enqueue((name, ty, id, localv))
          case _ => ???
        }
      case _ => () // ignored
    }
    for (s <- exprStatements)
      ExprParser.desalt(s) match {
        case let: LetDefStmt =>
          val pattern = let.defined
          val body = let.body.getOrElse {
            Reporter.report(MissingLetBody(let))
            ???
          }
          pattern match {
            case DefinedPattern(pattern, _) =>
              val ty = toTerm(let.ty match {
                case Some(ty) => c.given_Elab.inferType(ty).wellTyped
                case None     => newType
              })
              val wellTyped = toTerm(c.given_Elab.check(body, ty))
              pattern match {
                case PatternBind(name, meta) =>
                  if (let.kind == LetDefType.Let) {
                    val id = Uniqid.generate[LocalV]
                    val localv = LocalV(name.name, ty, id, convertMeta(meta))
                    val r = elab.collector.newSymbol(localv, id, let, context.ctx)
                    context.update(_.add(ContextItem(name.name, id, localv, ty, Some(r))).knownAdd(id, TyAndVal(ty, wellTyped)))
                    statements = statements :+ LetStmtTerm(localv, wellTyped, ty, convertMeta(let.meta))
                  } else {
                    val self = defs.dequeue()
                    assume(self.defined == name)
                    self.ty.fill(ty)
                    context.update(_.knownAdd(self.id, TyAndVal(ty, wellTyped)))
                    statements = statements :+ DefStmtTerm(self.localv, wellTyped, ty, convertMeta(let.meta))
                  }
                case _ => ???
              }
            case _ => ???
          }
        case e =>
          throw new UnsupportedOperationException("not implemented: " + e)
          ??? // for breakpoitn debugging
      }
    val resultExpr = block.result.getOrElse(UnitExpr(meta = None))
    val returning = toTerm(c.given_Elab.check(resultExpr, ty))
    assume(defs.isEmpty)
    // TODO: checking for possible leakage and do substitution for examples like {let a = Int; 1 : a}
    result.fill(BlockTerm(statements, returning, convertMeta(block.meta)))
    Result.Done
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
