package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.{Context, convertMeta}
import chester.utils.elab.*

import scala.language.postfixOps

trait Lit extends Kind

case object IntegerLit extends Lit {
  type Of = IntegerLit
}

case class IntegerLit(expr: IntegerLiteral, ty: CellRWOr[Term], result: CellRW[Term])(using ctx0: Context)
    extends Constraint(IntegerLit)
    with ConstraintTerm {
  given Context = ctx0
  def meta = convertMeta(expr.meta)
}

case object IntegerLitHandler extends Handler[ElabOps, IntegerLit.type](IntegerLit) {

  override def run(c: IntegerLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    if (ty <:? IntegerType(meta) isTrue) {
      result.fill(IntegerTerm(expr.value, meta))
      return Result.Done
    }
    if (expr.value >= 0 && (ty <:? NaturalType(meta) isTrue)) {
      result.fill(NaturalTerm(expr.value, meta))
      return Result.Done
    }
    if (expr.value.isValidInt) {
      result.fill(IntTerm(expr.value.toInt, meta))
      ty <:! IntType(meta)
      Result.Done
    } else {
      result.fill(IntegerTerm(expr.value, meta))
      ty <:! IntegerType(meta)
      Result.Done
    }
  }
}


case object StringLit extends Lit {
  type Of = StringLit
}
case class StringLit(expr: StringLiteral, ty: CellRWOr[Term], result: CellRW[Term])(using ctx0: Context)
    extends Constraint(StringLit)
    with ConstraintTerm {
  given Context = ctx0
  def meta = convertMeta(expr.meta)
}
case object StringLitHandler extends Handler[ElabOps, StringLit.type](StringLit) {

  override def run(c: StringLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    ty <:! StringType(meta)
    result.fill(StringTerm(expr.value, meta))
    Result.Done
  }
}