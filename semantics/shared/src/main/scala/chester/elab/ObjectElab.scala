package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*


case object ObjectElab extends Kind {
  type Of = ObjectElab
}

case class ObjectElab(obj: ObjectExpr, ty: CellRWOr[Term])(using elab: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(ObjectElab)
    with ConstraintTerm {

  override val result: CellRW[Term] = newHole

  given context: Context = ctx

  given Elab = elab
}

case object ObjectElabHandler extends Handler[ElabOps, ObjectElab.type](ObjectElab) {
  override def run(c: ObjectElab)(using elab: ElabOps, solver: SolverOps): Result =
    ???

  override def canDefaulting(level: DefaultingLevel): Boolean = ???
}
