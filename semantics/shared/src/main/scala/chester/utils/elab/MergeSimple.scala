package chester.utils.elab

import chester.tyck.TyckOps

case object MergeSimple extends Kind {
  type ConstraintType = MergeSimple[?]
}

case class MergeSimple[T](a: CellRW[T], b: CellRW[T]) extends Constraint(MergeSimple) {}

case object MergeSimpleHandler extends Handler[TyckOps](MergeSimple) {}
