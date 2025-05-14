package chester.elab

import chester.syntax.concrete.Expr
import chester.syntax.core.{IntegerType, Term}
import chester.tyck.convertMeta
import chester.utils.elab.*

trait Lit extends Kind

case object IntegerLit extends Lit {
  type Of = IntegerLit
}

case class IntegerLit(expr: Expr, ty: CellRWOr[Term], result: CellRW[Term]) extends Constraint(IntegerLit) with ConstraintTerm {
  def meta = convertMeta(expr.meta)
}

case object IntegerLitHandler extends Handler[ElabOps,IntegerLit.type](IntegerLit) {

  override def run(c: IntegerLit)(using ElabOps, SolverOps): Result = {
    if((c.ty <:? IntegerType(c.meta)).isTrue) {
      return Result.Done
    }
    ???
  }
}