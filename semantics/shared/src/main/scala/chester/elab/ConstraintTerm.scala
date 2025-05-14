package chester.elab

import chester.syntax.core.Term
import chester.utils.elab.{CellR, CellRW, ConstraintResult}

type ConstraintTerm = ConstraintResult[CellR[Term]]
type ConstraintTermRW = ConstraintResult[CellRW[Term]]
