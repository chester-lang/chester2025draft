package chester.elab

import chester.utils.elab.{CellR, Kind}
import io.github.iltotore.iron.Constraint

open trait ConstraintTerm(val kind: Kind) extends Constraint(kind) {
  def result: CellR[Term]
}