package chester.elab

import chester.syntax.core.Term
import chester.utils.elab.{CellR, Constraint}

 trait ConstraintTerm extends Constraint {
  def result: CellR[Term]
}