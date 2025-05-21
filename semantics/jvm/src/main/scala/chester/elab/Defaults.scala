package chester.elab

import chester.utils.elab.{ConcurrentSolver, HandlerConf, ProceduralSolver, SolverFactory}
import chester.utils.ifNativeImage
val PROCEDURAL_SOLVER = true
object Defaults {
  given Elab = DefaultElabImpl
  given SolverFactory = if (PROCEDURAL_SOLVER) ProceduralSolver else ifNativeImage(ProceduralSolver)(ConcurrentSolver)
  given HandlerConf[ElabOps] = DefaultSolverConf

}
