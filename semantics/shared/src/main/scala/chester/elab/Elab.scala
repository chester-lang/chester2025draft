package chester.elab

import chester.cell.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.HoldNotReadable
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
      case Some(v) => toTerm(v)
      case None    => MetaTerm(HoldNotReadable(c), meta = meta)
    }
}

trait Elab {

  def elab(expr: Expr, ty: CellRW[Term], effects: ReprEffects)(using
      localCtx: Context,
      ops: ElabOps,
      state: SolverOps
  ): Term

}

trait DefaultElab extends Elab {
  override def elab(expr: Expr, ty: CellRW[Term], effects: ReprEffects)(using localCtx: Context, ops: ElabOps, state: SolverOps): Term = expr match {
    case IntegerLiteral(i, meta) =>
      SolverOps.addConstraint(???)
      ???
    case _ => ???
  }
}
