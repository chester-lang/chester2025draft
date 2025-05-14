package chester.utils.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait Kind {
  def name: String = toString
  type Of <: Constraint
}

open trait Constraint(val kind: Kind) {
  def show: Vector[Term] = ???
}

open trait ConstraintResult[A] extends Constraint {
  def result: CellR[A]
}