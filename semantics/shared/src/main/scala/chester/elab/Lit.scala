package chester.elab

import chester.error.{LiteralTypeMismatch, Reporter}
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.{Context, convertMeta}
import chester.utils.elab.*

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
  def meta: Option[TermMeta] = convertMeta(expr.meta)
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
    if (PlatformInfo.isValidInt(expr.value) && (ty <:? IntType(meta) isTrue)) {
      result.fill(IntTerm(expr.value.toLong, meta))
      return Result.Done
    }
    if (JVMPlatformInfo.isValidUInt(expr.value.toLong) && (ty <:? UIntType(meta) isTrue)) {
      result.fill(UIntTerm(expr.value, meta))
      return Result.Done
    }
    toTerm(ty) match {
      case ty: MetaTerm[?] => Result.Waiting(assumeCell(ty))
      case _ =>
        result.fill(IntegerTerm(expr.value, meta))
        Reporter.report(LiteralTypeMismatch(IntegerTerm(expr.value, meta), toTerm(ty), expr))
        Result.Done
    }
  }

  override def defaulting(c: IntegerLit, level: DefaultingLevel)(using ElabOps, SolverOps): Boolean = {
    import c.*
    toTerm(ty) match {
      case ty: MetaTerm[?] =>
        if (expr.value.isValidInt) {
          assumeCell(ty).fill(IntType(meta))
        } else {
          assumeCell(ty).fill(IntegerType(meta))
        }
        true
      case _ => false
    }
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = level == DefaultingLevel.Lit
}

case object StringLit extends Lit {
  type Of = StringLit
}
case class StringLit(expr: StringLiteral, ty: CellRWOr[Term])(using ctx: Context, solverOps: SolverOps)
    extends Constraint(StringLit)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
  def meta: Option[TermMeta] = convertMeta(expr.meta)
}
case object StringLitHandler extends Handler[ElabOps, StringLit.type](StringLit) {

  override def run(c: StringLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    StringType(meta) <:! ty
    result.fill(StringTerm(expr.value, meta))
    Result.Done
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}

case object SymbolLit extends Lit {
  type Of = SymbolLit
}
case class SymbolLit(expr: SymbolLiteral, ty: CellRWOr[Term])(using ctx: Context, solverOps: SolverOps)
    extends Constraint(SymbolLit)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
  def meta: Option[TermMeta] = convertMeta(expr.meta)
}
case object SymbolLitHandler extends Handler[ElabOps, SymbolLit.type](SymbolLit) {

  override def run(c: SymbolLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    SymbolType(meta) <:! ty
    result.fill(SymbolTerm(expr.value, meta))
    Result.Done
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
