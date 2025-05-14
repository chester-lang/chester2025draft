package chester.elab

import chester.utils.elab.{MergeSimpleHandler, MutHandlerConf}

val DefaultSolverConf = new MutHandlerConf[ElabOps](MergeSimpleHandler,IntegerLitHandler)
