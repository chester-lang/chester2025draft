package chester.elab

import chester.cell.newPureEffects
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.Context
import chester.utils.elab.*

object DefaultElabImpl extends DefaultElab {}

case class Elaborator()(using elab: Elab, fac: SolverFactory, handlers: HandlerConf[ElabOps]) {
  def inferPure(expr: Expr, context: Context = Context.default)(using ElabOps): Judge = {
    given solver: SolverOps = fac(handlers)
    given Context = context.copy(effects = newPureEffects.toEffectsM)
    val ty = toTerm(newHole)
    val term = toTerm(elab.check(expr, ty))
    solver.run()
    Judge(term.zonkAll, ty.zonkAll, Effects.Empty)
  }
}

val DefaultElaborator = {
  import chester.elab.Defaults.given
  Elaborator()
}
