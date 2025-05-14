package chester.elab

import chester.syntax.concrete.Expr
import chester.syntax.core.Term
import chester.utils.elab.{CellRW, Constraint, Kind}

case object IntegerLit extends Kind

case class IntegerLit(expr: Expr, ty: CellRW[Term], result: CellRW[Term]) extends Constraint(IntegerLit) with ConstraintTerm {
  
}
