package chester.elab

import chester.utils.elab.{HandlerConf, MergeSimpleHandler, MutHandlerConf}

implicit val DefaultSolverConf: HandlerConf[ElabOps] = new MutHandlerConf[ElabOps](
  MergeSimpleHandler,
  IntegerLitHandler,
  StringLitHandler,
  SymbolLitHandler,
  PureHandler,
  UnifyHandler,
  IsTypeHandler,
  ListOfHandler,
  ListOfHandler,
  SimplifyUnionHandler,
  UnifyMultipleHandler,
  BlockElabHandler
)
