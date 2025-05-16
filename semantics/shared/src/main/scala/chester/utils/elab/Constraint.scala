package chester.utils.elab

/** implementations should be case object */
trait Kind {
  def name: String = toString
  type Of <: Constraint
}

trait Constraint(val kind: Kind) {}

trait ConstraintResult[+A] extends Constraint {
  def result: A
}
