package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*
import chester.utils.*

case object UnifyMultiple extends Kind {
  override type Of = UnifyMultiple
}

case class UnifyMultiple(
    lhs: CellRWOr[Term],
    rhs: Vector[CellRWOr[Term]]
)(using ctx: Context)
    extends Constraint(UnifyMultiple) {
  given Context = ctx
}

case object UnifyMultipleHandler
    extends Handler[ElabOps, UnifyMultiple.type](UnifyMultiple) {
  override def run(c: UnifyMultiple)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    val lhsV = toTerm(lhs)
    val rhsV = rhs.map(toTerm(_))
    if (lhsV.isInstanceOf[MetaTerm]) {
      return Result.Waiting(assumeCell(lhs))
    }
    if (rhsV.exists(_.isInstanceOf[MetaTerm])) {
      return Result.Waiting(assumeCell(rhsV.find(_.isInstanceOf[MetaTerm]).get))
    }
    val rhs1 = rhsV.filterNot(x => eqType(lhsV, x))
    if (rhs1.isEmpty) {
      return Result.Done
    }
    rhs1.foreach { rhs =>
      SolverOps.addConstraint(Unify(lhsV, rhs))
    }
    Result.Done
  }

  override def defaulting(c: UnifyMultiple, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    if(level != DefaultingLevel.UnifyMultipleMerge) return
    import c.{*, given}
    val lhsV = toTerm(lhs)
    val rhsV = rhs.map(toTerm(_))
    val rhs1 = rhsV.filterNot(x => eqType(lhsV, x)).distinctByEq(eqType)
    if(rhs1.isEmpty) {
      return
    }
    if(!lhsV.isInstanceOf[MetaTerm]) {
      for(rhs <- rhs1) {
        SolverOps.addConstraint(Unify(lhsV, rhs))
      }
      return
    }
  }
}