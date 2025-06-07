package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.*
import chester.error.Reporter
import chester.utils.elab.*

case object FunctionCallElab extends Kind {
  type Of = FunctionCallElab
}

case class FunctionCallElab(
    f: (wellTyped: CellRWOr[Term], ty: CellRWOr[Term]),
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
  override def run(c: FunctionCallElab)(using ElabOps, SolverOps): Result = {
    import c.*
    val f = toTerm(c.f.wellTyped)
    val fTy = toTerm(c.f.ty) match {
      case meta: MetaTerm[?] =>
        // If the function type is a meta term, we need to assume it
        return Result.Waiting(assumeCell(meta))
      case f: FunctionType => f
      case term =>
        Reporter.report(???)
        result.fill(???)
        return Result.Done
    }
    val fTyImplicit = fTy.telescopes.map(_.implicitly)
    val callImplicit = call.telescopes.map(_.implicitly)
    if (fTyImplicit.length == callImplicit.length && fTyImplicit.zip(callImplicit).forall { case (x, y) => x == y }) {
      // simplest case
      for ((defTele, callTele) <- fTy.telescopes.zip(call.telescopes)) {
        if (defTele.args.length == callTele.args.length) {
          // check the simplest case
          for ((defArg, callArg) <- defTele.args.zip(callTele.args))
            if (callArg.name.isEmpty || defArg.bind.exists(localv => localv.name == callArg.name.get.name)) {
              // simplest case, no different order of named arguments
              ???
            } else {
              throw new UnsupportedOperationException("not implemented yet: different order of named arguments")
            }
        } else {
          throw new UnsupportedOperationException("not implemented yet: default arguments in function calls")
        }
        ???
      }
      ???
    } else {
      throw new UnsupportedOperationException("not implemented yet: omitted implicit telescopes in function calls")
    }
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
