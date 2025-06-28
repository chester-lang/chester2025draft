package chester.elab

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.elab.*
import chester.error.*
import chester.utils.elab.*

import scala.util.boundary

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

  val context0: Context = ctx

  given elab: Elab = elab0
}

case object FunctionCallElabHandler extends Handler[ElabOps, FunctionCallElab.type](FunctionCallElab) {
  override def run(c: FunctionCallElab)(using ElabOps, SolverOps): Result = boundary {
    import c.{*, given}
    implicit var context: Context = c.context0
    val f = toTerm(c.f.wellTyped)
    val fTy = toTerm(c.f.ty) match {
      case meta: MetaTerm[?] =>
        // If the function type is a meta term, we need to assume it
        boundary.break(Result.Waiting(assumeCell(meta)))
      case f: FunctionType => f
      case term =>
        Reporter.report(TypeMismatch(FunctionType(Vector.empty, toTerm(ty), meta = None), term, call))
        result.fill(ErrorTerm(TypeMismatch(FunctionType(Vector.empty, toTerm(ty), meta = None), term, call), meta = None))
        boundary.break(Result.Done)
    }
    val fTyImplicit = fTy.telescopes.map(_.implicitly)
    val callImplicit = call.telescopes.map(_.implicitly)
    if (fTyImplicit.length == callImplicit.length && fTyImplicit.lazyZip(callImplicit).forall((x, y) => x == y)) {
      // simplest case
      var callings: Vector[Calling] = Vector.empty
      for ((defTele, callTele) <- fTy.telescopes.zip(call.telescopes)) {
        if (defTele.args.length == callTele.args.length) {
          // check the simplest case
          var calling: Vector[CallingArgTerm] = Vector.empty
          for ((defArg, callArg) <- defTele.args.zip(callTele.args))
            if (callArg.name.isEmpty || defArg.bind.exists(localv => localv.name == callArg.name.get.name)) {
              // simplest case, no different order of named arguments
              val wellTyped = elab.check(callArg.expr, defArg.ty)
              if (defArg.bind.isDefined) {
                val bind = defArg.bind.get
                context =
                  context.add(ContextItem(bind.name, bind.uniqId, bind, defArg.ty)).knownAdd(bind.uniqId, TyAndVal(defArg.ty, toTerm(wellTyped)))
              }
              calling = calling :+ CallingArgTerm(
                toTerm(wellTyped),
                defArg.ty,
                name = defArg.bind.map(_.name),
                meta = convertMeta(callArg.meta)
              )
            } else {
              throw new UnsupportedOperationException("not implemented yet: different order of named arguments")
            }

          assume(defTele.implicitly == callTele.implicitly)
          callings = callings :+ Calling(calling, defTele.implicitly, meta = convertMeta(callTele.meta))
        } else {
          throw new UnsupportedOperationException("not implemented yet: default arguments in function calls")
        }
        SolverOps.addConstraint(Unify(ty, fTy.resultTy, cause = call)) // todo: substitute by the current context?
        result.fill(FCallTerm(f, callings, meta = convertMeta(call.meta)))
        boundary.break(Result.Done)
      }
      ???
    } else {
      throw new UnsupportedOperationException("not implemented yet: omitted implicit telescopes in function calls")
    }
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
