package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object ListOf extends Kind {
  type Of = ListOf
}
case class ListOf(items: Vector[(wellTyped: CellROr[Term], ty: CellROr[Term])], ty: CellRWOr[Term])(using ctx: Context, ops: SolverOps)
    extends Constraint(ListOf)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
}

case object ListOfHandler extends Handler[ElabOps, ListOf.type](ListOf) {
  override def run(c: ListOf)(using ElabOps, SolverOps): Result =
    ???
}
