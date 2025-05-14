package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*
import chester.utils.assumeNonEmpty

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
  override def run(c: ListOf)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    toTerm(ty) match {
      case ty: MetaTerm => Result.Waiting(assumeCell(ty))
      case ListType(ty, meta) =>
        SolverOps.addConstraint(IsType(result))
        if (items.isEmpty) return Result.Done
        val allTys = items.map(_.ty)
        val unioned = SolverOps.useConstraint(SimplifyUnion(allTys.map(_.toTerm()).assumeNonEmpty))
        unioned <:! ty
        result.fill(ListTerm(items.map(_.wellTyped.toTerm()), meta))
        Result.Done
    }
  }
  override def defaulting(c: ListOf, level: DefaultingLevel)(using ElabOps, SolverOps): Unit ={
    if(level != DefaultingLevel.ListOfSetListType) return
    import c.{*, given}
    val ty1 = assumeCell(ty)
    if (items.isEmpty) {
      val innerTy = toTerm(SolverOps.useConstraint(IsType(newHole)))
      ty1 <:! ListType(innerTy, meta=None)
      return
    }
    val allTys = items.map(_.ty)
    val unioned = SolverOps.useConstraint(SimplifyUnion(allTys.map(_.toTerm()).assumeNonEmpty))
    ty1 <:! ListType(toTerm(unioned), meta=None)
  }
}
