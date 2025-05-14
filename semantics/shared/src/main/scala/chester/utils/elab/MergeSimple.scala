package chester.utils.elab

case object MergeSimple extends Kind {
  type Of = MergeSimple[?]
}

case class MergeSimple[T](a: CellRW[T], b: CellRW[T]) extends Constraint(MergeSimple) {}

case object MergeSimpleHandler extends Handler[Any, MergeSimple.type](MergeSimple) {
  override def run(m: MergeSimple[?])(using Any, SolverOps): Result = (SolverOps.readUnstable(m.a), SolverOps.readUnstable(m.b)) match {
    case (None, None) => Result.Waiting(m.a, m.b)
    case (Some(a), None) =>
      SolverOps.fill(m.b, a)
      Result.Done
    case (None, Some(b)) =>
      SolverOps.fill(m.a, b)
      Result.Done
    case (Some(a), Some(b)) =>
      if (a == b) {
        Result.Done
      } else {
        Result.Failed
      }
  }
}
