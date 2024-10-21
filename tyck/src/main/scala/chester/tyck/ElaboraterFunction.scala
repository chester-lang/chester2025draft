package chester.tyck

import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector
import chester.uniqid.*

trait ElaboraterFunction extends ProvideCtx with Elaborater {
  def elabFunction(
      expr: FunctionExpr,
      ty: CellId[Term],
      outerEffects: CIdOf[EffectsCell]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term
}

trait ProvideElaboraterFunction extends ElaboraterFunction {
  def elabArg(arg: Arg, effects: CIdOf[EffectsCell])(using
      localCtx: MutableContext,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): ArgTerm = {
    require(arg.decorations.isEmpty, "decorations are not supported yet")
    val ty = elabTy(arg.ty)
    val default = arg.exprOrDefault.map(elab(_, ty, effects))
    val id = UniqId.generate[LocalV]
    val bind = newLocalv(arg.name.name, ty, id, arg.meta)
    val r = parameter.newSymbol(bind, id, arg, localCtx)
    localCtx.update(_.add(ContextItem(arg.name.name, id, bind, ty, Some(r))))
    default match {
      case Some(defaultValue) =>
        ArgTerm(bind, ty, Some(defaultValue), arg.vararg, meta = None)
      case None =>
        ArgTerm(bind, ty, None, arg.vararg, meta = None)
    }
  }

  def elabTelescope(telescope: DefTelescope, effects: CIdOf[EffectsCell])(using
      localCtx: MutableContext,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): TelescopeTerm = {
    // Process each argument in the telescope, updating the context
    val argTerms = telescope.args.map { arg =>
      elabArg(arg, effects)
    }

    TelescopeTerm(argTerms, telescope.implicitly, meta = None)
  }

  def elabFunction(
      expr: FunctionExpr,
      ty: CellId[Term],
      outerEffects: CIdOf[EffectsCell]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = {
    // Start with a mutable local context based on the current context
    val mutableCtx = new MutableContext(ctx)

    val effects = newEffects

    // Elaborate each telescope and collect TelescopeTerms
    val telescopeTerms: Vector[TelescopeTerm] = expr.telescope.map { telescope =>
      elabTelescope(telescope, effects)(using
        mutableCtx,
        parameter,
        ck,
        state
      )
    }

    // Process the return type, if provided
    val returnType: Term = expr.resultTy match {
      case Some(rtExpr) =>
        (checkType((rtExpr))(using mutableCtx.ctx, parameter, ck, state))
      case None =>
        newTypeTerm(using ck, state)
    }

    // Process the body of the function using the updated context
    val bodyTerm: Term = elab(expr.body, returnType, effects)(using
      mutableCtx.ctx,
      parameter,
      ck,
      state
    )

    // Build the function type by folding over the telescopes
    val functionType =
      FunctionType(telescopeTerms, returnType, effects = toTerm(effects), meta = None)

    // Unify the expected type with the constructed function type
    unify(ty, functionType, expr)

    Function(functionType, bodyTerm, meta = None)
  }
}
