package chester.elab

import chester.syntax.core.Term

case object MergeSimple extends Kind {
  type ConstraintType = MergeSimple[?]
}

case class MergeSimple[T](a: CellId[T], b: CellId[T]) extends Constraint(MergeSimple) {
  override def show: Vector[Term] = ???
}

case object MergeSimpleHandler extends Handler(MergeSimple) {
  
}