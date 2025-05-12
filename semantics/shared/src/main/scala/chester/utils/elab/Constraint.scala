package chester.utils.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait Kind {
  override def toString: String = super.toString
  type ConstraintType <: Constraint
}


open trait Constraint(val kind: Kind) {
  def show: Vector[Term]
}

enum Result {
  case Done
  case Failed
  case Waiting(vars: Vector[CellId[?]])
}


open trait Handler[Ops](val kind: Kind) {
  def run(constant: kind.ConstraintType)(using Ops): Result = ???
  def zonk(constant: kind.ConstraintType, level: ZonkLevel)(using Ops): Unit = ()
}

enum ZonkLevel extends Enum[ZonkLevel] {
  case Least
  case Upper
}
extension (x: ZonkLevel) {
  // depend on this assumption: the first one should be 0
  def precedence: Int = x.ordinal
}