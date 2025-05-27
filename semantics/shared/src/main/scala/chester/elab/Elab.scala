package chester.elab

import chester.cell.*
import chester.error.Reporter
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.InMeta
import chester.utils.cell.{LiteralCellContent, OnceCellContent}
import chester.utils.elab.*

import scala.annotation.tailrec

@tailrec
def toTerm(x: CellRW[Term] | CellR[Term] | Term, meta: Option[TermMeta] = None)(using SolverOps): Term = x match {
  case MetaTerm(c: InMeta[CellRW[Term] @unchecked], meta) if SolverOps.hasStableValue(c.inner) => toTerm(c.inner, meta)

  case x: Term => x
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v) => toTerm(v, meta)
      case None    => MetaTerm(InMeta(c), meta = meta)
    }
}

def toTermRec(x: CellRW[Term] | CellR[Term] | Term, meta: Option[TermMeta] = None)(using SolverOps): Term = toTerm(x).descentRec {
  case x: MetaTerm =>
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
      case Some(v: MetaTerm) => toCell(v, meta)
      case _                 => c
    }
  case MetaTerm(c: InMeta[CellRW[Term] @unchecked], meta) => toCell(c.inner, meta)
  case x: Term                                            => SolverOps.addCell(LiteralCellContent(x))
}

@tailrec
def assumeCell(x: CellRWOr[Term], meta: Option[TermMeta] = None)(using SolverOps): CellRW[Term] = x match {
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v: MetaTerm) => assumeCell(v, meta)
      case _                 => c
    }
  case MetaTerm(c: InMeta[CellRW[Term] @unchecked], meta) => assumeCell(c.inner, meta)
  case x: Term                                            => throw new IllegalArgumentException("Not a cell?")
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

  def inferType(expr: Expr)(using
      ctx: Context,
      _1: ElabOps,
      _2: SolverOps
  ): (wellTyped: CellRWOr[Term], ty: CellRWOr[Term]) = {
    given Context = ctx.copy(effects = newPureEffects.toEffectsM)
    val i = infer(expr)
    (SolverOps.callConstraint(IsType(i.wellTyped)), i.ty)
  }

  def checkWholeUnit(fileName: String, block: Block)(using ctx: Context, _1: ElabOps, _2: SolverOps): CellRWOr[TAST] = ???

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
      case _ @ListExpr(xs, meta) =>
        val items = xs.map(infer(_))
        SolverOps.callConstraint(ListOf(items, ty))
      case b: Block => SolverOps.callConstraint(BlockElab(b, ty))
      case id: Identifier =>
        ctx.get(id.name) match {
          case Some(item) =>
            ty >:! item.ty
            item.ref
          case None =>
            Reporter.report(???)
            ErrorTerm(???, meta = convertMeta(id.meta))
        }
      case _ => ???
    }
}
