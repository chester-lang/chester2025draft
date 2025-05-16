package chester.elab

import chester.error.Reporter
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.*
import chester.utils.elab.*
import spire.math.Trilean

case object Unify extends Kind {
  override type Of = Unify
}

// if unify failed failback to next or fail if next is None
case class Unify(lhs: CellRWOr[Term], rhs: CellRWOr[Term], next: Option[Constraint] = None)(using ctx: Context) extends Constraint(Unify) {
  given Context = ctx
}

case object UnifyHandler extends Handler[ElabOps, Unify.type](Unify) {
  private def failed(c: Unify)(using ElabOps, SolverOps): Result = c.next match {
    case Some(next) =>
      SolverOps.addConstraint(next)
      Result.Done
    case None =>
      Reporter.report(???)
      Result.Done
  }
  override def run(c: Unify)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    (rhs <:? lhs) match {
      case Trilean.True    => return Result.Done
      case Trilean.False   => return failed(c)
      case Trilean.Unknown => ()
    }
    val lhsV = toTerm(lhs)
    val rhsV = toTerm(rhs)
    if (lhsV.isInstanceOf[MetaTerm]) {
      return Result.Waiting(assumeCell(lhs))
    }
    if (rhsV.isInstanceOf[MetaTerm]) {
      return Result.Waiting(assumeCell(rhs))
    }
    (lhsV, rhsV) match {
      case (AnyType(level, _), _)                  => Result.Done // TODO: check for level
      case (_: SimpleType, AnyType(level, _))      => failed(c)
      case (_, NothingType(_))                     => Result.Done
      case (ListType(lhs, meta), ListType(rhs, _)) =>
        // For debug lhs1 and rhs1
        val lhs1 = toTerm(lhs)
        val rhs1 = toTerm(rhs)
        SolverOps.addConstraint(Unify(lhs1, rhs1, next = next))
        Result.Done
      case (ListType(_, meta), _: SimpleType) => failed(c)
      case (lhs: MetaTerm, _)                 => Result.Waiting(assumeCell(lhs))
      case (_, rhs: MetaTerm)                 => Result.Waiting(assumeCell(rhs))
      case (Union(lhs, _), Union(rhs, rhsMeta)) =>
        val lhs1 = lhs.map(toTerm(_))
        val rhs1 = rhs.map(toTerm(_))
        val common = lhs1.filter(x => rhs1.exists(y => eqType(x, y)))
        val lhs2 = lhs1.filterNot(x => common.exists(y => eqType(x, y)))
        val rhs2 = rhs1.filterNot(x => common.exists(y => eqType(x, y)))
        if (rhs2.isEmpty) {
          return Result.Done
        }
        if (lhs2.isEmpty) {
          return failed(c)
        }
        if (lhs2.length == 1 && rhs2.length == 1) {
          SolverOps.addConstraint(Unify(lhs2.head, rhs2.head, next = next))
          return Result.Done
        }
        if (lhs2.length == 1) {
          SolverOps.addConstraint(Unify(lhs2.head, Union(rhs2.assumeNonEmpty, rhsMeta), next = next))
          return Result.Done
        }
        ???
      case (lhs, Union(rhs, _)) =>
        rhs.foreach(rhs => SolverOps.addConstraint(Unify(lhs, rhs, next = next)))
        Result.Done
      case _ => ???
    }
  }

  override def defaulting(c: Unify, level: DefaultingLevel)(using elabops: ElabOps, solverOps: SolverOps): Boolean = {
    if (level != DefaultingLevel.UnifyMerge) return false
    import c.*
    val lhsV = toTerm(lhs)
    val rhsV = toTerm(rhs)
    (lhsV, rhsV) match {
      case (MetaTerm(InMeta[CellRW[Term] @unchecked](lhs), meta), rhs) => lhs.fill(rhs)
      case (lhs, MetaTerm(InMeta[CellRW[Term] @unchecked](rhs), meta)) => rhs.fill(lhs)
      case _                                                           => ()
    }
    true
  }
}
