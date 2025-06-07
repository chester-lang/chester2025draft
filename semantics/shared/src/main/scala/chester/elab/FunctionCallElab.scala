package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.*
import chester.utils.elab.*

case object FunctionCallElab extends Kind {
  type Of = FunctionCallElab
}

case class FunctionCallElab(
    call: DesaltFunctionCall,
    ty: CellRWOr[Term]
)(using elab0: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(FunctionCallElab)
    with ConstraintTerm {

  override val result: CellRW[Term] = newHole

  given context: Context = ctx

  given elab: Elab = elab0
}

case object FunctionCallElabHandler extends Handler[ElabOps, FunctionCallElab.type](FunctionCallElab) {
  override def run(c: FunctionCallElab)(using ElabOps, SolverOps): Result =
    ???

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
