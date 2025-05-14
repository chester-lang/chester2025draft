package chester.utils.elab

case object MergeSimple extends Kind {
  type Of = MergeSimple[?]
}

case class MergeSimple[T](a: CellRW[T], b: CellRW[T]) extends Constraint(MergeSimple) {}

case object MergeSimpleHandler extends Handler[Any,MergeSimple.type](MergeSimple) {
  override def run(constant: MergeSimple[?])(using Any, SolverOps): Result = ???
}
