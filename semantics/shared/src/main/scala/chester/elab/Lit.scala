package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.{Context, convertMeta}
import chester.utils.elab.*
import chester.utils.elab.Result.Failed
import spire.math.UInt

import scala.language.postfixOps

trait Lit extends Kind

case object IntegerLit extends Lit {
  type Of = IntegerLit
}

case class IntegerLit(expr: IntegerLiteral, ty: CellRWOr[Term])(using ctx: Context, ops: SolverOps)
    extends Constraint(IntegerLit)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
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
    if (expr.value.isValidInt && (ty <:? IntType(meta) isTrue)) {
      result.fill(IntTerm(expr.value.toInt, meta))
      return Result.Done
    }
    if (expr.value >= 0 && expr.value.isValidLong && expr.value.toLong <= UInt.MaxValue.toLong && (ty <:? UIntType(meta) isTrue)) {
      result.fill(UIntTerm(UInt(expr.value.toLong), meta))
      return Result.Done
    }
    toTerm(ty) match {
      case ty: MetaTerm => Result.Waiting(assumeCell(ty))
      case _            => {
        result.fill(IntegerTerm(expr.value, meta))
        Failed
      }
    }
  }

  override def defaulting(c: IntegerLit, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    import c.{*, given}
    if (expr.value.isValidInt) {
      ty <:! IntType(meta)
    } else {
      ty <:! IntegerType(meta)
    }
  }
}

case object StringLit extends Lit {
  type Of = StringLit
}
case class StringLit(expr: StringLiteral, ty: CellRWOr[Term])(using ctx: Context, solverOps: SolverOps)
    extends Constraint(StringLit)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
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

case object SymbolLit extends Lit {
  type Of = SymbolLit
}
case class SymbolLit(expr: SymbolLiteral, ty: CellRWOr[Term])(using ctx: Context, solverOps: SolverOps)
    extends Constraint(SymbolLit)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
  def meta = convertMeta(expr.meta)
}
case object SymbolLitHandler extends Handler[ElabOps, SymbolLit.type](SymbolLit) {

  override def run(c: SymbolLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    ty <:! SymbolType(meta)
    result.fill(SymbolTerm(expr.value, meta))
    Result.Done
  }
}
