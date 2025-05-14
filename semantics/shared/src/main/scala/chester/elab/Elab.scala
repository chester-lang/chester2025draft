package chester.elab

import chester.cell.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

trait Elab {

  def elab(expr: Expr, ty: CellRW[Term], effects: ReprEffects)(using
                                                               localCtx: Context,
                                                               ops: ElabOps,
                                                               state: SolverOps
  ): Term

}

trait DefaultElab extends Elab {
  override def elab(expr: Expr, ty: CellRW[Term], effects: ReprEffects)(using localCtx: Context, ops: ElabOps, state: SolverOps): Term = expr match {
    case IntegerLiteral(i, meta) => {
      SolverOps.addConstraint(???)
      ???
    }
    case _ => ???
  }
}