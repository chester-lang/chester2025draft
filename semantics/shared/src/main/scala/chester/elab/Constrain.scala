package chester.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait Kind {
  override def toString: String = super.toString
  type ConstrainType <: Constrain
}


open trait Constrain(val kind: Kind) {
  def show: Vector[Term]
}

case class CellId[T <: Term]()

open trait Handler(val kind: Kind) {
  def run: Unit
  def zonk(level: ZonkLevel): Unit
}

enum ZonkLevel extends Enum[ZonkLevel] {
  case Least
  case Upper
}
extension (x: ZonkLevel) {
  // depend on this assumption: the first one should be 0
  def precedence: Int = x.ordinal
}