package chester.elab

import chester.cell.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.InMeta
import chester.utils.cell.{LiteralCellContent, OnceCellContent}
import chester.utils.elab.*

import scala.annotation.tailrec

@tailrec
def toTerm(x: CellRW[Term] | CellR[Term] | Term, meta: Option[TermMeta] = None)(using  SolverOps): Term = x match {
  case MetaTerm(c: InMeta[CellRW[Term] @unchecked], meta) if SolverOps.hasStableValue(c.inner) => toTerm(c.inner, meta)

  case x: Term => x
  case c: CellRW[Term @unchecked] =>
    SolverOps.readStable(c) match {
      case Some(v) => toTerm(v, meta)
      case None    => MetaTerm(InMeta(c), meta = meta)
    }
}

implicit class ToTermOps(private val x: CellRW[Term] | CellR[Term] | Term) extends AnyVal  {
  def toTerm(meta: Option[TermMeta] = None)(using  SolverOps): Term = chester.elab.toTerm(x, meta)
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

def newHole[T<:Term](using SolverOps): CellRW[T] = SolverOps.addCell(OnceCellContent[T]())

trait Elab {

  def elab(expr: Expr, ty: CellRWOr[Term])(using
      effects: CellEffects,
      localCtx: Context,
      ops: ElabOps,
      state: SolverOps
  ): CellROr[Term]

  def infer(expr: Expr)(using
       CellEffects,
       Context,
       ElabOps,
       SolverOps
  ): (wellTyped: CellROr[Term], ty: CellRWOr[Term]) = {
    val ty = SolverOps.callConstraint(IsType(newHole))
    val result = elab(expr, ty)
    (result, ty)
  }

}

trait DefaultElab extends Elab {
  given Elab = this
  override def elab(expr: Expr, ty: CellRWOr[Term])(using effects: CellEffects, localCtx: Context, ops: ElabOps, state: SolverOps): CellROr[Term] =
    expr match {
      case expr: IntegerLiteral =>
        SolverOps.addConstraint(Pure(effects))
        SolverOps.callConstraint(IntegerLit(expr, ty))
      case expr: StringLiteral =>
        SolverOps.addConstraint(Pure(effects))
        SolverOps.callConstraint(StringLit(expr, ty))
      case expr: SymbolLiteral =>
        SolverOps.addConstraint(Pure(effects))
        SolverOps.callConstraint(SymbolLit(expr, ty))
      case expr @ ListExpr(xs, meta) =>
        val items = xs.map(infer(_))
        SolverOps.callConstraint(ListOf(items, ty))
      case b: Block => SolverOps.callConstraint(BlockElab(b, ty))
      case _ => ???
    }
}
