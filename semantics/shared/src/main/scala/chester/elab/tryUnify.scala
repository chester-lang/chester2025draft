package chester.elab

import chester.syntax.core.Term
import chester.tyck.Context
import chester.utils.elab.*
import spire.math.Trilean

extension (sub: CellRWOr[Term]) {
  def `<:?`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Trilean = tryUnify(supertype, sub)
  def `<:!`(supertype: CellRWOr[Term])(using ElabOps, SolverOps, Context): Unit = SolverOps.addConstraint(Unify(supertype, sub))
}

def tryUnify(lhs: CellRWOr[Term], rhs: CellRWOr[Term])(using ElabOps, SolverOps, Context): Trilean = ???

case object Unify extends Kind {
  override type Of = Unify
}

case class Unify(lhs: CellRWOr[Term], rhs: CellRWOr[Term])(using ctx: Context) extends Constraint(Unify) {
  given Context = ctx
}

case object UnifyHandler extends Handler[ElabOps, Unify.type](Unify) {
  override def run(c: Unify)(using ElabOps, SolverOps): Result = ???
}
