package chester.tyck

import chester.error.{Reporter, TyckProblem}

given ckToReport(using ck: TyckOps): Reporter[TyckProblem] = ck.reporter

type TyckOps = StateReporter[TyckProblem, Unit]
