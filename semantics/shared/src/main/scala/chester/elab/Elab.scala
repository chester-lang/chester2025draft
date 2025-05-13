package chester.elab

import chester.cell.*
import chester.syntax.concrete.Expr
import chester.syntax.core.Term
import chester.tyck.{Context, TyckOps}
import chester.tyck.api.SemanticCollector
import chester.utils.elab.*

trait Elab {

  def elab(expr: Expr, ty: CellReprOfRWOr[Term], effects: CellReprOfRW[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: TyckOps,
      state: SolverOps
  ): Term

}
