package chester.elab

import chester.cell.*
import chester.syntax.concrete.Expr
import chester.syntax.core.Term
import chester.tyck.Context
import chester.utils.elab.*

trait Elab {

  def elab(expr: Expr, ty: ReprRW[Term], effects: ReprEffects)(using
                                                                         localCtx: Context,
                                                                         ops: ElabOps,
                                                                         state: SolverOps
  ): Term

}

trait DefaultElab extends Elab {
  override def elab(expr: Expr, ty: ReprRW[Term], effects: ReprEffects)(using localCtx: Context, ops: ElabOps, state: SolverOps): Term = ???
}