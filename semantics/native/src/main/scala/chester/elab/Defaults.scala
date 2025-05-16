package chester.elab

import chester.utils.elab.*

import com.eed3si9n.ifdef.*

object Defaults {
  given Elab = DefaultElabImpl
  @ifndef("scalaNativeNoMultithread")
  given SolverFactory = ConcurrentSolver // ProceduralSolver
  @ifdef("scalaNativeNoMultithread")
  given SolverFactory = ProceduralSolver
  given HandlerConf[ElabOps] = DefaultSolverConf

}
