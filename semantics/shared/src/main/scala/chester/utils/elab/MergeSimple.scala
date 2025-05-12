package chester.utils.elab

import chester.syntax.core.Term
import chester.tyck.TyckOps

case object MergeSimple extends Kind {
  type ConstraintType = MergeSimple[?]
}

case class MergeSimple[T](a: CellId[T], b: CellId[T]) extends Constraint(MergeSimple) {
  override def show: Vector[Term] = ???
}

case object MergeSimpleHandler extends Handler[TyckOps](MergeSimple) {
  
}