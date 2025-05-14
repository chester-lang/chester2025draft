package chester.elab

import chester.cell.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.HoldNotReadable
import chester.utils.cell.{LiteralCellContent, OnceCellContent}
import chester.utils.elab.*

import scala.annotation.tailrec

@tailrec
def toTerm(x: CellRWOr[Term], meta: Option[TermMeta] = None)(using SolverOps): Term = x match {
  case x: Term =>
    x match {
      case MetaTerm(c: HoldNotReadable[CellRW[Term] @unchecked], meta) if SolverOps.hasSomeValue(c.inner) => toTerm(c.inner, meta)
      case x: Term                                                                                        => x
    }
  case c: CellRW[Term @unchecked] =>
    SolverOps.readUnstable(c) match {
      case Some(v) => toTerm(v, meta)
      case None    => MetaTerm(HoldNotReadable(c), meta = meta)
    }
}

@tailrec
def toCell(x: CellRWOr[Term], meta: Option[TermMeta] = None)(using SolverOps): CellRW[Term] = x match {
  case c: CellRW[Term @unchecked] =>
    SolverOps.readUnstable(c) match {
      case Some(v: MetaTerm) => toCell(v, meta)
      case _                 => c
    }
  case MetaTerm(c: HoldNotReadable[CellRW[Term] @unchecked], meta) => toCell(c.inner, meta)
  case x: Term                                                     => SolverOps.addCell(LiteralCellContent(x))
}

def newHole(using SolverOps): CellRW[Term] = SolverOps.addCell(OnceCellContent[Term]())

trait Elab {

  def elab(expr: Expr, ty: CellRWOr[Term], effects: CellEffects)(using
      localCtx: Context,
      ops: ElabOps,
      state: SolverOps
  ): CellROr[Term]

}

trait DefaultElab extends Elab {
  override def elab(expr: Expr, ty: CellRWOr[Term], effects: CellEffects)(using localCtx: Context, ops: ElabOps, state: SolverOps): CellROr[Term] =
    expr match {
      case expr: IntegerLiteral =>
        SolverOps.addConstraint(Pure(effects))
        SolverOps.useConstraint(IntegerLit(expr, ty, newHole))
      case _ => ???
    }
}
