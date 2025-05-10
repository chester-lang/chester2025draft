package chester.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait ConstrainKind {
  override def toString: String = super.toString
  type Rule <: Constrain
}


open trait Constrain(val kind: ConstrainKind) {
  def show: Vector[Term]
}

case class CellId[T <: Term]()