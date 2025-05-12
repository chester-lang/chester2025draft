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
  def run(constant: kind.ConstraintType)(using Ops, SolverOps): Result = ???
  def zonk(constant: kind.ConstraintType, level: ZonkLevel)(using Ops, SolverOps): Unit = ()
}

enum ZonkLevel extends Enum[ZonkLevel] {
  case First
  case ZonkEverything
}
object ZonkLevel {
  val Values: Vector[ZonkLevel] = ZonkLevel.values.toVector.sortBy(_.precedence)
}
extension (x: ZonkLevel) {
  // depend on this assumption: the first one should be 0
  def precedence: Int = x.ordinal
}