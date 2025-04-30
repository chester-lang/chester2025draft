package chester.tyck

import chester.error.{Reporter, TyckProblem}

given ckToReport(using ck: TyckSession): Reporter[TyckProblem] = ck.reporter

type TyckSession = StateReporter[TyckProblem, Unit]
