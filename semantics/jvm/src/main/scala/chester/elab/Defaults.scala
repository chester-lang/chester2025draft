package chester.elab

import chester.utils.elab.{ConcurrentSolver, HandlerConf, SolverFactory}

object Defaults {
  given Elab = DefaultElabImpl
  given SolverFactory = ConcurrentSolver // ProceduralSolver
  given HandlerConf[ElabOps] = DefaultSolverConf

}
