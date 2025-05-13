package chester.utils.elab

import chester.tyck.TyckOps

case object MergeSimple extends Kind {
  type ConstraintType = MergeSimple[?]
}

case class MergeSimple[T](a: CellReprOfRW[T], b: CellReprOfRW[T]) extends Constraint(MergeSimple) {}

case object MergeSimpleHandler extends Handler[TyckOps](MergeSimple) {}
