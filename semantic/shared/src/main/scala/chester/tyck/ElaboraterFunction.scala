package chester.tyck

import cats.implicits.*
import chester.error.{Problem, Reporter, TyckProblem, VectorReporter, WithServerity}
import chester.syntax.{Name, LoadedModules, ModuleRef, TAST, DefaultModule}
import chester.syntax.concrete.{Expr, ExprMeta, Block, ObjectExpr, ObjectClause, FunctionExpr, DesaltFunctionCall, Arg, DefTelescope}
import chester.syntax.core.{Term, Effects, Meta, Typeω, LocalV, ReferenceCall, TelescopeTerm, ArgTerm, FunctionType, Function, Type, FCallTerm, Calling, CallingArgTerm, ErrorTerm}
import chester.syntax.core.spec.given_TypeF_Term_Type
import chester.syntax.core.spec.given
import chester.tyck.api.{SemanticCollector, SymbolCollector}
import chester.utils.{MutBox, flatMapOrdered, hasDuplication, assumeNonEmpty}
import chester.utils.propagator.{StateAbility, Propagator, ZonkResult, ProvideCellId, Cell}
import chester.reduce.{Reducer, NaiveReducer, ReduceContext, ReduceMode}
import chester.reduce.ReduceContext.given_Conversion_Context_ReduceContext
import chester.uniqid.{Uniqid, UniqidOf}
import chester.resolve.{SimpleDesalt, resolveOpSeq}

import scala.language.implicitConversions

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
  // Flag to enable or disable termination checking
  val terminationCheckEnabled: Boolean = true // Set to false to disable termination checking

  def elabArg(arg: Arg, effects: CIdOf[EffectsCell])(using
      localCtx: MutableContext,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): ArgTerm = {
    require(arg.decorations.isEmpty, "decorations are not supported yet")
    // First elaborate the type
    val ty = elabTy(arg.ty)(using localCtx.ctx, parameter, ck, state)
    // Try type-level reduction on the type
    given ReduceContext = localCtx.ctx.toReduceContext
    given Reducer = localCtx.ctx.given_Reducer
    val reducedTy = summon[Reducer].reduce(ty, ReduceMode.TypeLevel) match {
      case Type(level, _) =>
        // If we have a Type term, try to reduce its level
        val reducedLevel = summon[Reducer].reduce(level, ReduceMode.TypeLevel)
        if (reducedLevel != level) {
          Type(reducedLevel, ty.meta)
        } else {
          // If type-level reduction didn't work, try normal reduction
          val normalReducedLevel = summon[Reducer].reduce(level, ReduceMode.Normal)
          if (normalReducedLevel != level) {
            Type(normalReducedLevel, ty.meta)
          } else {
            // Try to evaluate the level term if it's a function call
            level match {
              case fcall: FCallTerm =>
                // Try to evaluate the function call
                val reducedF = summon[Reducer].reduce(fcall.f, ReduceMode.TypeLevel)
                val reducedArgs = fcall.args.map(calling =>
                  Calling(
                    calling.args.map(arg => CallingArgTerm(
                      summon[Reducer].reduce(arg.value, ReduceMode.TypeLevel),
                      summon[Reducer].reduce(arg.ty, ReduceMode.TypeLevel),
                      arg.name,
                      arg.vararg,
                      arg.meta
                    )),
                    calling.implicitly,
                    calling.meta
                  )
                )
                reducedF match {
                  case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
                    // Substitute args into body
                    val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
                      telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
                        acc.substitute(param.bind, arg.value)
                      }
                    }
                    // Try reducing the substituted body
                    val evaluated = summon[Reducer].reduce(substitutedBody, ReduceMode.TypeLevel)
                    if (evaluated != substitutedBody) {
                      Type(evaluated, ty.meta)
                    } else {
                      ty
                    }
                  case _ => ty
                }
              case _ => ty
            }
          }
        }
      case fcall: FCallTerm =>
        // If we have a function call, try to evaluate it in type-level mode first
        val reducedFCall = summon[Reducer].reduce(fcall, ReduceMode.TypeLevel)
        if (reducedFCall != fcall) {
          reducedFCall
        } else {
          // If type-level reduction didn't work, try normal reduction
          val normalReducedFCall = summon[Reducer].reduce(fcall, ReduceMode.Normal)
          if (normalReducedFCall != fcall) {
            normalReducedFCall
          } else {
            // Try to evaluate the function call
            val reducedF = summon[Reducer].reduce(fcall.f, ReduceMode.TypeLevel)
            val reducedArgs = fcall.args.map(calling =>
              Calling(
                calling.args.map(arg => CallingArgTerm(
                  summon[Reducer].reduce(arg.value, ReduceMode.TypeLevel),
                  summon[Reducer].reduce(arg.ty, ReduceMode.TypeLevel),
                  arg.name,
                  arg.vararg,
                  arg.meta
                )),
                calling.implicitly,
                calling.meta
              )
            )
            reducedF match {
              case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
                // Substitute args into body
                val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
                  telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
                    acc.substitute(param.bind, arg.value)
                  }
                }
                // Try reducing the substituted body
                val evaluated = summon[Reducer].reduce(substitutedBody, ReduceMode.TypeLevel)
                if (evaluated != substitutedBody) {
                  evaluated
                } else {
                  ty
                }
              case _ => ty
            }
          }
        }
      case ref: ReferenceCall =>
        // If we have a reference, try to resolve it
        val resolved = localCtx.ctx.toReduceContext.resolve(ref)
        if (resolved != ref) {
          // Try to reduce the resolved reference
          val reducedResolved = summon[Reducer].reduce(resolved, ReduceMode.TypeLevel)
          if (reducedResolved != resolved) {
            reducedResolved
          } else {
            val normalReducedResolved = summon[Reducer].reduce(resolved, ReduceMode.Normal)
            if (normalReducedResolved != resolved) {
              normalReducedResolved
            } else {
              // Try to evaluate if it's a function call
              resolved match {
                case fcall: FCallTerm =>
                  val reducedF = summon[Reducer].reduce(fcall.f, ReduceMode.TypeLevel)
                  val reducedArgs = fcall.args.map(calling =>
                    Calling(
                      calling.args.map(arg => CallingArgTerm(
                        summon[Reducer].reduce(arg.value, ReduceMode.TypeLevel),
                        summon[Reducer].reduce(arg.ty, ReduceMode.TypeLevel),
                        arg.name,
                        arg.vararg,
                        arg.meta
                      )),
                      calling.implicitly,
                      calling.meta
                    )
                  )
                  reducedF match {
                    case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
                      // Substitute args into body
                      val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
                        telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
                          acc.substitute(param.bind, arg.value)
                        }
                      }
                      // Try reducing the substituted body
                      val evaluated = summon[Reducer].reduce(substitutedBody, ReduceMode.TypeLevel)
                      if (evaluated != substitutedBody) {
                        evaluated
                      } else {
                        ty
                      }
                    case _ => ty
                  }
                case _ => ty
              }
            }
          }
        } else {
          ty
        }
      case _ => ty
    }
    
    // Use the reduced type for the binding but keep the original type in the ArgTerm
    val default = arg.exprOrDefault.map(elab(_, reducedTy, effects)(using localCtx.ctx, parameter, ck, state))
    val id = Uniqid.generate[LocalV]
    val bind = newLocalv(arg.name.name, reducedTy, id, arg.meta)
    val r = parameter.newSymbol(bind, id, arg, localCtx.ctx)
    localCtx.update(_.add(ContextItem(arg.name.name, id, bind, reducedTy, Some(r))))
    default match {
      case Some(defaultValue) =>
        ArgTerm(bind, ty, Some(defaultValue), arg.vararg, meta = None)
      case None =>
        ArgTerm(bind, ty, None, arg.vararg, meta = None)
    }
  }

  def elabTelescope(telescope: DefTelescope, effects: CIdOf[EffectsCell])(using
      mutableCtx: MutableContext,
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
        checkType(rtExpr)(using mutableCtx.ctx, parameter, ck, state)
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

    // Extract the function name from 'expr.name', if available
    val functionNameOpt: Option[String] = None // TODO // placeholder

    // Termination check logic (can be easily removed or disabled)
    if (terminationCheckEnabled) {
      // placeholder
      val isTerminating = analyzeTermination(bodyTerm, functionNameOpt)
      if (!isTerminating) {
        val problem = PotentialNonterminatingFunction(expr)
        ck.reporter(problem)
      }
    }

    Function(functionType, bodyTerm, meta = None)
  }

  // placeholder, broken code
  // Simple termination analysis by traversing the Term using inspectRecursive
  def analyzeTermination(
      term: Term,
      functionNameOpt: Option[String]
  )(using
      Tyck,
      StateAbility[Tyck]
  ): Boolean = {
    // Collect function calls within the term
    val functionCalls = collectFunctionCalls(term)

    functionNameOpt match {
      case Some(functionName) =>
        // If the function calls itself, consider it potentially non-terminating
        !functionCalls.contains(functionName)
      case None =>
        // Cannot determine the function name; assume it terminates
        true
    }
  }

  // Helper function to collect function call names from a Term using inspectRecursive
  def collectFunctionCalls(term: Term): Set[String] = {
    val calls = scala.collection.mutable.Set[String]()

    term.inspectRecursive {
      {
        case FCallTerm(function, _, _) =>
          function match {
            // TODO: calls += name
            case _ =>
              // Continue traversing the function term
              function.inspectRecursive {
                // TODO: calls += name
                _ =>
              }
          }
        case _ =>
      }
    }

    calls.toSet
  }
}
