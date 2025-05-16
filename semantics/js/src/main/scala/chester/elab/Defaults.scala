package chester.elab

import chester.utils.elab.*

object Defaults {
  given Elab = DefaultElabImpl
  given SolverFactory = ProceduralSolver
  given HandlerConf[ElabOps] = DefaultSolverConf

}
