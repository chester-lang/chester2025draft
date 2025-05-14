package chester.utils.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait Kind {
  def name: String = toString
  type ConstraintType <: Constraint
}

open trait Constraint(val kind: Kind) {
  def show: Vector[Term] = ???
}
