package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object ListOf extends Kind {
  type Of = ListOf
}
case class ListOf(items: Vector[(wellTyped: CellROr[Term], ty: CellRWOr[Term])], ty: CellRWOr[Term])(using ctx: Context, ops: SolverOps)
    extends Constraint(ListOf)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
}

case object ListOfHandler extends Handler[ElabOps, ListOf.type](ListOf) {
  override def run(c: ListOf)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    toTerm(ty) match {
      case ty: MetaTerm => Result.Waiting(assumeCell(ty))
      case ListType(ty, meta) =>
        SolverOps.addConstraint(IsType(ty))
        if (items.isEmpty) {
          result.fill(ListTerm(Vector(), meta))
          return Result.Done
        }
        val allTys = items.map(_.ty)
        SolverOps.addConstraint(UnifyMultiple(ty, allTys))
        result.fill(ListTerm(items.map(_.wellTyped.toTerm()), meta))
        Result.Done
    }
  }
  override def defaulting(c: ListOf, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    if (level != DefaultingLevel.ListOfSetListType) return
    import c.{*, given}
    assumeCell(ty).fill(ListType(toTerm(SolverOps.callConstraint(IsType(newHole))), meta = None))
  }
}
