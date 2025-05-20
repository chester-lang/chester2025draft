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

case object UnifyMultipleHandler extends Handler[ElabOps, UnifyMultiple.type](UnifyMultiple) {
  override def run(c: UnifyMultiple)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    val lhsV = toTermRec(lhs)
    val rhsV = rhs.map(toTermRec(_))
    val rhs1 = cleanUpUnion(rhsV.filterNot(x => eqType(lhsV, x)).assumeNonEmpty)
    if (rhs1.isEmpty) {
      return Result.Done
    }
    val wait = rhs1.find(_.isInstanceOf[MetaTerm])
    if (wait.isDefined) return Result.Waiting(assumeCell(wait.get))
    val (alllist0, rhs2) = rhs1.partition(_.isInstanceOf[ListType])
    val alllist = alllist0.map(_.asInstanceOf[ListType])
    val rhs3 = if (alllist.nonEmpty) {
      val mergedlist = SolverOps.callConstraint(SimplifyUnion(alllist.map(_.ty).assumeNonEmpty))
      rhs2 :+ ListType(toTermRec(mergedlist), meta = None)
    } else {
      rhs2
    }
    SolverOps.addConstraint(Unify(lhsV, Union1(cleanUpUnion(rhs3.map(toTermRec(_)).assumeNonEmpty), meta = None)))
    Result.Done
  }

  override def defaulting(c: UnifyMultiple, level: DefaultingLevel)(using ElabOps, SolverOps): Boolean = {
    import c.{*, given}
    val lhsV = toTermRec(lhs)
    val rhsV = rhs.map(toTermRec(_))
    val rhs1 = rhsV.filterNot(x => eqType(lhsV, x)).distinctByEq(eqType)
    if (rhs1.isEmpty) {
      return false
    }
    if (!lhsV.isInstanceOf[MetaTerm]) {
      for (rhs <- rhs1)
        SolverOps.addConstraint(Unify(lhsV, rhs))
      true
    } else {
      false
    }
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = level == DefaultingLevel.UnifyMultipleMerge
}
