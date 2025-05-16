package chester.elab

import chester.utils.elab.{ConcurrentSolver, HandlerConf, ProceduralSolver, SolverFactory}
import chester.utils.ifNativeImage

object Defaults {
  given Elab = DefaultElabImpl
  given SolverFactory = ifNativeImage(ProceduralSolver)(ConcurrentSolver)
  given HandlerConf[ElabOps] = DefaultSolverConf

}
