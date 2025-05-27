package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*
import spire.math.Trilean

import scala.language.postfixOps

extension (term: CellRWOr[Term]) {
  def `<:?`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Trilean = tryUnify(supertype, term)
  def `<:!`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Unit = SolverOps.addConstraint(Unify(supertype, term))
  def `>:!`(subtype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Unit = SolverOps.addConstraint(Unify(term, subtype))
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
