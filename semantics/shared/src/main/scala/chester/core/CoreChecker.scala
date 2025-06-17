package chester.core

import chester.error.*
import chester.syntax.core.*

case class CheckerContext()

object CoreChecker {
  def infer(term: Term)(using CheckerContext, Reporter[TyckProblem]): Term = ???
  def check(term: Term, ty: Term)(using CheckerContext, Reporter[TyckProblem]): Unit = ???
}
