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
    import c.{*, given}
    val exprStatements = block.statements.map(resolve(_)(using c.context))
    // separate val and given definitions for better debugging in intellij idea
    val context: MutableContext = new MutableContext(c.context)
    given MutableContext = context
    var statements: Vector[StmtTerm] = Vector()
    val defs: mutable.Queue[(defined: Identifier, ty: CellRW[Term], id: UniqidOf[LocalVar], localv: LocalVar)] = mutable.Queue()
    exprStatements.foreach {
      case defstmt: LetDefStmt if defstmt.kind == LetDefType.Def =>
        defstmt.defined match {
          case DefinedPattern(PatternBind(name, meta), _) =>
            val ty = newType
            val id = Uniqid.make[LocalVar]
            val localv = LocalVar(name.name, toTerm(ty), id, convertMeta(meta))
            val r = elab.collector.newSymbol(localv, id, defstmt, context.ctx)
            context.update(_.add(ContextItem(name.name, id, localv, toTerm(ty), Some(r))))
            defs.enqueue((name, ty, id, localv))
          case DefinedFunction(name, _, meta) =>
            val ty = newType
            val id = Uniqid.make[LocalVar]
            val localv = LocalVar(name.name, toTerm(ty), id, convertMeta(meta))
            val r = elab.collector.newSymbol(localv, id, defstmt, context.ctx)
            context.update(_.add(ContextItem(name.name, id, localv, toTerm(ty), Some(r))))
            defs.enqueue((name, ty, id, localv))
        }
      case _ => () // ignored
    }
    for (s <- exprStatements)
      ExprParser.desalt(s) match {
        case extension: ExtensionStmt =>
          if (
            extension.telescope.length == 1 && extension.telescope.head.isInstanceOf[DefTelescope] && extension.telescope.head
              .asInstanceOf[DefTelescope]
              .implicitly == false && extension.telescope.head.asInstanceOf[DefTelescope].args.length == 1
          ) {
            val arg: Arg = extension.telescope.head.asInstanceOf[DefTelescope].args.head
            (arg.name, arg.ty) match {
              case (Some(name), Some(ty)) =>
                val (evaledTy, sort) = summon[Elab].inferType(ty)
                val ty1 = summon[Elab].reduceForTyUntyped(evaledTy)
                val localvar: LocalVar = LocalVar(name.name, ty1, Uniqid.make[LocalVar], meta = name.meta)
                if (extension.body.result.nonEmpty) {
                  ???
                }
                val definitions = extension.body.statements.map(x => ExprParser.desalt(x)).map {
                  case let: LetDefStmt if let.kind == LetDefType.Def =>
                    ???
                    ???
                  case stmt =>
                    Reporter.report(???)
                    ???
                }
                val result: ExtensionDefinition = ExtensionDefinition(ty = ty1, bind = localvar, methods = ???)
                throw new UnsupportedOperationException("extension: not implemented: " + extension)
              case _ =>
                throw new UnsupportedOperationException("extension: not implemented: " + extension)
            }
          } else {
            throw new UnsupportedOperationException("extension: not implemented: " + extension)
          }
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
                    val id = Uniqid.make[LocalVar]
                    val localv = LocalVar(name.name, ty, id, convertMeta(meta))
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
            case DefinedFunction(name, telescopes, meta) => {
              val (wellTyped, ty) = c.given_Elab.infer(???)
              ???
            }
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
