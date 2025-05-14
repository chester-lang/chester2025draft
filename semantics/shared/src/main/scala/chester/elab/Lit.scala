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

  override def defaulting(constant: IntegerLit, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    
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
