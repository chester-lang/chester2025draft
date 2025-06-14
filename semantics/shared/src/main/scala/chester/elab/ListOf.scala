package chester.elab

import chester.elab.{ElabOps, UnifyMultiple}
import chester.syntax.core.*
import chester.elab.Context
import chester.syntax.concrete.ListExpr
import chester.utils.elab.*

case object ListOf extends Kind {
  type Of = ListOf
}
case class ListOf(items: Vector[(wellTyped: CellROr[Term], ty: CellRWOr[Term])], ty: CellRWOr[Term], cause: ListExpr)(using
    ctx: Context,
    ops: SolverOps
) extends Constraint(ListOf)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
}

case object ListOfHandler extends Handler[ElabOps, ListOf.type](ListOf) {
  override def run(c: ListOf)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    toTerm(ty) match {
      case ty: MetaTerm[?] => Result.Waiting(assumeCell(ty))
      case ListType(ty, meta) =>
        SolverOps.addConstraint(IsType(ty))
        if (items.isEmpty) {
          result.fill(ListTerm(Vector(), meta))
          return Result.Done
        }
        val allTys = items.map(_.ty)
        SolverOps.addConstraint(UnifyMultiple(ty, allTys, cause))
        result.fill(ListTerm(items.map(_.wellTyped.toTerm()), meta))
        Result.Done
    }
  }
  override def defaulting(c: ListOf, level: DefaultingLevel)(using ElabOps, SolverOps): Boolean = {
    import c.{*, given}
    assumeCell(ty).fill(ListType(toTerm(newType), meta = None))
    true
  }

  override def canDefaulting(level: DefaultingLevel): Boolean =
    level == DefaultingLevel.ListOfSetListType
}
