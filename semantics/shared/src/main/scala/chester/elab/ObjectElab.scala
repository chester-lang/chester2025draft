package chester.elab

import chester.error.unreachable
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.*
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
  override def run(c: ObjectElab)(using ElabOps, SolverOps): Result = {
    import c.{*, given}
    // TODO: different elab rules for more cases like when use lile a hashmap
    val xs = obj.clauses.map {
      case _: ObjectExprClause             => unreachable("ObjectExprClause should already be resolved")
      case clause: ObjectExprClauseOnValue => (elab.inferPure(clause.key), elab.infer(clause.value), clause.meta)
    }
    val types = xs.map { case (k, v, meta) => ObjectClauseValueTerm(toTerm(k.wellTyped), toTerm(v.ty), meta) }
    val values = xs.map { case (k, v, meta) => ObjectClauseValueTerm(toTerm(k.wellTyped), toTerm(v.wellTyped), meta) }
    val ty1 = ObjectType(
      types,
      meta = obj.meta
    )
    SolverOps.addConstraint(Unify(ty, toTerm(ty1)))
    result.fill(ObjectTerm(values, meta = convertMeta(obj.meta)))
    Result.Done
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
