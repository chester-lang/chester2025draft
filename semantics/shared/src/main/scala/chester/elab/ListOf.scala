package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object ListOf extends Kind {
  type Of = ListOf
}
case class ListOf(items: Vector[(wellTyped: CellROr[Term], ty: CellROr[Term])], ty: CellRWOr[Term], result: CellRW[Term])(using ctx: Context)
    extends Constraint(ListOf)
    with ConstraintTerm {
  given Context = ctx
}

case object ListOfHandler extends Handler[ElabOps, ListOf.type](ListOf) {
  override def run(c: ListOf)(using ElabOps, SolverOps): Result = {
    ???
  }
}