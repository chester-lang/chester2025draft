package chester.elab

import chester.cell.newPureEffects
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.{SolverFactory, *}
import chester.tyck.LocalCtxOps

implicit object DefaultElabImpl extends DefaultElab {}

extension (t: Term) {
  def zonkAll(using SolverOps): Term = t.descentRec {
    case t: MetaTerm =>
      toTerm(t) match {
        case _: MetaTerm => throw new IllegalStateException("Zonked term is still a MetaTerm")
        case t: Term     => t.zonkAll
      }
    case t: Term => t
  }
}

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
