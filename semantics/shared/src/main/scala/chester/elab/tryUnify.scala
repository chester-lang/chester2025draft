package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.InMeta
import chester.utils.elab.*
import spire.math.Trilean

import scala.language.postfixOps

extension (sub: CellRWOr[Term]) {
  def `<:?`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Trilean = tryUnify(supertype, sub)
  def `<:!`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Unit = SolverOps.addConstraint(Unify(supertype, sub))
}

def tryUnify(lhs: CellRWOr[Term], rhs: CellRWOr[Term])(using ElabOps, SolverOps, Context): Trilean = {
  if (lhs == rhs) return Trilean.True
  val lhsV = toTerm(lhs)
  val rhsV = toTerm(rhs)
  if (lhsV == rhsV) return Trilean.True
  (lhsV, rhsV) match {
    case (IntegerType(_), IntegerType(_)) => Trilean.True
    case (IntegerType(_), _: SimpleType)  => Trilean.False
    case (IntType(_), IntType(_))         => Trilean.True
    case (IntType(_), _: SimpleType)      => Trilean.False
    case (NaturalType(_), NaturalType(_)) => Trilean.True
    case (NaturalType(_), _: SimpleType)  => Trilean.False
    case (StringType(_), StringType(_))   => Trilean.True
    case (StringType(_), _: SimpleType)   => Trilean.False
    case (Union(xs, meta), rhs) =>
      if (xs.exists(lhs => tryUnify(lhs, rhs).isTrue)) return Trilean.True
      if (xs.forall(lhs => tryUnify(lhs, rhs).isFalse)) return Trilean.False
      Trilean.Unknown
    case (lhs, Intersection(ys, meta)) =>
      if (ys.exists(rhs => tryUnify(lhs, rhs).isTrue)) return Trilean.True
      if (ys.forall(rhs => tryUnify(lhs, rhs).isFalse)) return Trilean.False
      Trilean.Unknown
    case _ =>
      Trilean.Unknown
  }
}

case object Unify extends Kind {
  override type Of = Unify
}

case class Unify(lhs: CellRWOr[Term], rhs: CellRWOr[Term])(using ctx: Context) extends Constraint(Unify) {
  given Context = ctx
}

case object UnifyHandler extends Handler[ElabOps, Unify.type](Unify) {
  override def run(c: Unify)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    if (rhs <:? lhs isTrue) return Result.Done
    val lhsV = toTerm(lhs)
    val rhsV = toTerm(rhs)
    if (lhsV.isInstanceOf[MetaTerm]) {
      return Result.Waiting(assumeCell(lhs))
    }
    if (rhsV.isInstanceOf[MetaTerm]) {
      return Result.Waiting(assumeCell(rhs))
    }
    (lhsV, rhsV) match {
      case (ListType(lhs, meta), ListType(rhs, meta2)) =>
        // For debug lhs1 and rhs1
        val lhs1 = toTerm(lhs)
        val rhs1 = toTerm(rhs)
        SolverOps.addConstraint(Unify(lhs1, rhs1))
        Result.Done
      case (lhs: MetaTerm, rhs) => Result.Waiting(assumeCell(lhs))
      case (lhs, rhs: MetaTerm) => Result.Waiting(assumeCell(rhs))
    }
  }

  override def defaulting(c: Unify, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    import c.*
    val lhsV = toTerm(lhs)
    val rhsV = toTerm(rhs)
    (lhsV, rhsV) match {
      case (MetaTerm(InMeta[CellRW[Term] @unchecked](lhs), meta), rhs) => lhs.fill(rhs)
      case (lhs, MetaTerm(InMeta[CellRW[Term] @unchecked](rhs), meta)) => rhs.fill(lhs)
      case _                                                           => ()
    }
  }
}
