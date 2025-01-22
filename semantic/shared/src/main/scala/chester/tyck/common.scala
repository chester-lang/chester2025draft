package chester.tyck

import chester.error.{Reporter, TyckProblem}

given ckToReport(using ck: Tyck): Reporter[TyckProblem] = ck.reporter

type Tyck = Get[TyckProblem, Unit]
