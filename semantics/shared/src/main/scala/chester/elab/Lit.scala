package chester.elab

import chester.syntax.concrete.Expr
import chester.syntax.core.{IntegerType, Term}
import chester.tyck.{Context, convertMeta}
import chester.utils.elab.*

import scala.language.postfixOps

trait Lit extends Kind

case object IntegerLit extends Lit {
  type Of = IntegerLit
}

case class IntegerLit(expr: Expr, ty: CellRWOr[Term], result: CellRW[Term])(using ctx0: Context) extends Constraint(IntegerLit) with ConstraintTerm {
  given Context = ctx0
  def meta = convertMeta(expr.meta)
}

case object IntegerLitHandler extends Handler[ElabOps, IntegerLit.type](IntegerLit) {

  override def run(c: IntegerLit)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    if (ty <:? IntegerType(meta) isTrue) {
      return Result.Done
    }
    ???
  }
}
