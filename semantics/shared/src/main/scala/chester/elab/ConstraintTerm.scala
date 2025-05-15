package chester.elab

import chester.syntax.core.Term
import chester.utils.elab.{CellRW, ConstraintResult}

type ConstraintTerm = ConstraintResult[CellRW[Term]]

