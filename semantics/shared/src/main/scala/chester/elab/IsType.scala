package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

import scala.language.implicitConversions

case object IsType extends Kind {
  type Of = IsType[Nothing]
}

case class IsType[W](result: CellOf[Term, W] | Term)(using ctx: Context) extends Constraint(IsType) with ConstraintResult[CellOf[Term, W] | Term] {
  given Context = ctx
}

case object IsTypeHandler extends Handler[ElabOps, IsType.type](IsType) {
  implicit inline def writeTerm[W](inline x: CellOf[Term, W] | Term): CellRWOr[Term] = x.asInstanceOf[CellRWOr[Term]]
  override def run(c: IsType[Nothing])(using ElabOps, SolverOps): Result = {
    import c.*
    toTerm(result) match {
      case _: MetaTerm => Result.Waiting(assumeCell(result))
      case _           => Result.Done
    }
  }

  override def defaulting(constant: IsType[Nothing], level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    if (level != DefaultingLevel.IsType) return
    import constant.*
    toTerm(result) match {
      case result: MetaTerm =>
        assumeCell(result).fill(AnyType0)
      case _ =>
      // do nothing
    }
  }
}
