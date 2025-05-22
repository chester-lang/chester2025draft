package chester.utils.elab

import chester.error.{MergeError, MissingImplementation, Reporter, TyckProblem}
import chester.syntax.concrete.EmptyExpr

case object MergeSimple extends Kind {
  type Of = MergeSimple[?]
}

case class MergeSimple[T](a: CellRW[T], b: CellRW[T]) extends Constraint(MergeSimple) {}

case object MergeSimpleHandler extends Handler[Reporter[TyckProblem], MergeSimple.type](MergeSimple) {
  override def run(m: MergeSimple[?])(using Reporter[TyckProblem], SolverOps): Result =
    (SolverOps.readUnstable(m.a), SolverOps.readUnstable(m.b)) match {
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
          // For term values, provide better error message
          (a, b) match {
            case (aTerm: chester.syntax.core.Term, bTerm: chester.syntax.core.Term) =>
              Reporter.report(MergeError(aTerm, bTerm))
            case _ =>
              Reporter.report(MissingImplementation(s"Merge for values of type ${a.getClass.getSimpleName}", EmptyExpr))
          }
          Result.Done
        }
    }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
