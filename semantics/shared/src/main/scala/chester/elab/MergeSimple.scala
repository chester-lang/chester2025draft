package chester.elab

import chester.syntax.core.Term

case object MergeSimple extends ConstrainKind {
  type Rule = MergeSimple[?]
}

case class MergeSimple[T <: Term](a: CellId[T], b: CellId[T]) extends Constrain(MergeSimple) {

  override def show: Vector[Term] = ???
}
