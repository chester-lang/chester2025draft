package chester.elab

import chester.error.unreachable
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object ObjectElab extends Kind {
  type Of = ObjectElab
}

case class ObjectElab(obj: ObjectExpr, ty: CellRWOr[Term])(using elab0: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(ObjectElab)
    with ConstraintTerm {

  override val result: CellRW[Term] = newHole

  given context: Context = ctx

  given elab: Elab = elab0
}

case object ObjectElabHandler extends Handler[ElabOps, ObjectElab.type](ObjectElab) {
  override def run(c: ObjectElab)(using elabOps: ElabOps, solver: SolverOps): Result = {
    import c.{*, given}
    val xs = obj.clauses.map {
      case _: ObjectExprClause             => unreachable("ObjectExprClause should already be resolved")
      case clause: ObjectExprClauseOnValue => (elab.infer(clause.key), elab.infer(clause.value))
    }
    ???
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
