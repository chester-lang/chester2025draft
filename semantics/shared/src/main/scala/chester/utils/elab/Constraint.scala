package chester.utils.elab


/** implementations should be case object */
open trait Kind {
  def name: String = toString
  type Of <: Constraint
}

open trait Constraint(val kind: Kind) {
}

open trait ConstraintResult[A] extends Constraint {
  def result: A
}
