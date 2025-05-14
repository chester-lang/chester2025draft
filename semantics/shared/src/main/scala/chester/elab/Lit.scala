package chester.elab

import chester.syntax.concrete.Expr
import chester.syntax.core.Term
import chester.utils.elab.{CellRW, CellRWOr, Constraint, Kind}

trait Lit extends Kind

case object IntegerLit extends Lit {
  type Of = IntegerLit
}

case class IntegerLit(expr: Expr, ty: CellRWOr[Term], result: CellRW[Term]) extends Constraint(IntegerLit) with ConstraintTerm {}
