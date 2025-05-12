package chester.elab

import chester.tyck.TyckOps
import chester.utils.elab.{MergeSimpleHandler, MutHandlerConf}

val DefaultSolverConf = new MutHandlerConf[TyckOps](MergeSimpleHandler)
