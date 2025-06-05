package chester.elab

import chester.cell.*
import chester.elab.*
import chester.error.{Reporter, TyckProblem, VectorReporter}
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.syntax.{DefaultModule, ModuleRef}
import chester.elab.Context
import chester.utils.HoldNotReadable
import chester.utils.cell.{LiteralCellContent, OnceCellContent}
import chester.utils.elab.*

import scala.annotation.tailrec

@tailrec
def toTermUnstable[T <: Term](x: CellRW[T] | CellR[T] | T, meta: Option[TermMeta] = None)(using SolverOps): Term = x match {
  case m @ MetaTerm(_, meta) if SolverOps.hasSomeValue(m.unsafeRead[CellRW[T] | CellR[T]]) =>
    toTermUnstable(m.unsafeRead[CellRW[T] | CellR[T]], meta)

  case x: Term => x
  case c: CellRW[Term @unchecked] =>
    SolverOps.readUnstable(c) match {
      case Some(v) => toTermUnstable(v.asInstanceOf[CellRW[T] | CellR[T] | T], meta)
      case None    => MetaTerm(HoldNotReadable(c), meta = meta)
    }
}

@tailrec
def toTerm(x: CellRW[Term] | CellR[Term] | Term, meta: Option[TermMeta] = None)(using SolverOps): Term = x match {
  case MetaTerm(c: HoldNotReadable[CellRW[Term] @unchecked], meta) if SolverOps.hasStableValue(c.inner) => toTerm(c.inner, meta)

  case x: Term => x
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v) => toTerm(v, meta)
      case None    => MetaTerm(HoldNotReadable(c), meta = meta)
    }
}

def toTermRec(x: CellRW[Term] | CellR[Term] | Term, meta: Option[TermMeta] = None)(using SolverOps): Term = toTerm(x).descentRec {
  case x: MetaTerm[?] =>
    val updated = toTerm(x, x.meta)
    if (updated == x) x else toTermRec(updated, updated.meta)
  case t => t
}

implicit class ToTermOps(private val x: CellRW[Term] | CellR[Term] | Term) extends AnyVal {
  def toTerm(meta: Option[TermMeta] = None)(using SolverOps): Term = chester.elab.toTerm(x, meta)
}

@tailrec
def toCell(x: CellRWOr[Term], meta: Option[TermMeta] = None)(using SolverOps): CellRW[Term] = x match {
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v: MetaTerm[?]) => toCell(v, meta)
      case _                    => c
    }
  case m @ MetaTerm(_, meta) => toCell(m.unsafeRead[CellRW[Term]], meta)
  case x: Term               => SolverOps.addCell(LiteralCellContent(x))
}

@tailrec
def assumeCell(x: CellRWOr[Term], meta: Option[TermMeta] = None)(using SolverOps): CellRW[Term] = x match {
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v: MetaTerm[?]) => assumeCell(v, meta)
      case _                    => c
    }
  case m @ MetaTerm(_, meta) => assumeCell(m.unsafeRead[CellRW[Term]], meta)
  case x: Term               => throw new IllegalArgumentException("Not a cell?")
}

def newHole[T <: Term](using SolverOps): CellRW[T] = SolverOps.addCell(OnceCellContent[T]())

def newType(using SolverOps, Context): CellRW[Term] = toCell(SolverOps.callConstraint(IsType[Term, Term](newHole)))

trait Elab {

  def check(expr: Expr, ty: CellRWOr[Term])(using
      ctx: Context,
      ops: ElabOps,
      state: SolverOps
  ): CellRWOr[Term]

  def infer(expr: Expr)(using
      Context,
      ElabOps,
      SolverOps
  ): (wellTyped: CellRWOr[Term], ty: CellRWOr[Term]) = {
    val ty = newType
    val result = check(expr, ty)
    (result, ty)
  }

  def inferPure(expr: Expr)(using
      ctx: Context,
      _1: ElabOps,
      _2: SolverOps
  ): (wellTyped: CellRWOr[Term], ty: CellRWOr[Term]) = {
    given ctx1: Context = ctx.copy(effects = newPureEffects.toEffectsM)
    infer(expr)(using ctx1, _1, _2)
  }

  def inferType(expr: Expr)(using
      ctx: Context,
      _1: ElabOps,
      _2: SolverOps
  ): (wellTyped: CellRWOr[Term], ty: CellRWOr[Term]) = {
    given Context = ctx.copy(effects = newPureEffects.toEffectsM)
    val i = infer(expr)
    (SolverOps.callConstraint(IsType(i.wellTyped)), i.ty)
  }

  def checkWholeUnit(fileName: String, block: Block)(using ctx: Context, _1: ElabOps, _2: SolverOps): TAST = {
    val (module, blk): (ModuleRef, Block) = resolve(block) match {
      case b @ Block(head +: heads, tail, _) =>
        resolve(head) match {
          case ModuleStmt(module, meta) => (module, Block(heads, tail, meta))
          case _                        => (DefaultModule, b)
        }
      case expr => (DefaultModule, Block(Vector(), Some(expr), expr.meta))
    }
    val effects = newDynamicEffects.toEffectsM
    val ctx1 = ctx.updateModule(module).copy(effects = effects)
    val reporter = new VectorReporter[TyckProblem]()
    val (wellTyped, ty) = infer(blk)(using ctx1, _1.copy(reporter = reporter), _2)
    TAST(
      fileName = fileName,
      module = module,
      ast = toTerm(wellTyped),
      ty = toTerm(ty),
      effects = effects,
      problems = () => reporter.getReports
    )
  }

}

trait DefaultElab extends Elab {
  given Elab = this
  override def check(expr: Expr, ty: CellRWOr[Term])(using ctx: Context, ops: ElabOps, state: SolverOps): CellRWOr[Term] =
    resolve(expr) match {
      case expr: IntegerLiteral =>
        SolverOps.addConstraint(Pure(ctx.effects))
        SolverOps.callConstraint(IntegerLit(expr, ty))
      case expr: StringLiteral =>
        SolverOps.addConstraint(Pure(ctx.effects))
        SolverOps.callConstraint(StringLit(expr, ty))
      case expr: SymbolLiteral =>
        SolverOps.addConstraint(Pure(ctx.effects))
        SolverOps.callConstraint(SymbolLit(expr, ty))
      case expr @ ListExpr(xs, meta) =>
        val items = xs.map(infer(_))
        SolverOps.callConstraint(ListOf(items, ty, expr))
      case b: Block => SolverOps.callConstraint(BlockElab(b, ty))
      case expr @ DesaltFunctionCall(Identifier("__native", _), telescopes, meta) =>
        telescopes match {
          case Vector(
                DesaltCallingTelescope(Seq(CallingArg(None, ty0, false, _), CallingArg(None, s @ StringLiteral(code, _), false, _)), false, _)
              ) =>
            val ty1 = inferType(ty0)
            SolverOps.addConstraint(Unify(ty, ty1.wellTyped, expr))
            NativeTerm(StringTerm(code, s.meta), toTerm(ty1.wellTyped), meta = meta)
          case _ =>
            Reporter.report(???)
            ErrorTerm(???, meta = meta)
        }
      case OpSeq(Vector(Identifier("?", _), holeName), meta) =>
        holeName match {
          case Identifier(name, _) =>
            SolverOps.addConstraint(IsType(ty))
            HoleTerm(name = name, ty = toTerm(ty), meta = meta)
          case _ =>
            Reporter.report(???)
            SolverOps.addConstraint(IsType(ty))
            HoleTerm(name = "hole", ty = toTerm(ty), meta = meta)
        }
      case id: Identifier =>
        ctx.get(id.name) match {
          case Some(item) =>
            SolverOps.addConstraint(Unify(ty, item.ty, id))
            item.ref
          case None =>
            Reporter.report(???)
            ErrorTerm(???, meta = convertMeta(id.meta))
        }
      case expr: ObjectExpr => SolverOps.callConstraint(ObjectElab(expr, ty))
      case expr: UnitExpr =>
        SolverOps.addConstraint(Pure(ctx.effects))
        SolverOps.addConstraint(Unify(ty, UnitType(convertMeta(expr.meta)), expr))
        UnitTerm_(convertMeta(expr.meta))
      case expr: Expr =>
        val _ = expr
        ???
    }
}
