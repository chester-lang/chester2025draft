package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.syntax.core.{*, given}
import chester.tyck.api.SemanticCollector
import chester.reduce.{Reducer, ReduceContext, NaiveReducer}
import chester.utils.*
import chester.utils.propagator.*
import chester.tyck.*
import chester.syntax.{AbsoluteRef, ModuleRef}

trait ElaboraterFunctionCall extends ProvideCtx with Elaborater {
  def elabFunctionCall(
      expr: DesaltFunctionCall,
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term
}

trait ProvideElaboraterFunctionCall extends ElaboraterFunctionCall {
  override def elabFunctionCall(
      expr: DesaltFunctionCall,
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = {
    val functionExpr = elab(expr.function, newTypeTerm, effects)

    functionExpr match {
      case recordDef: RecordStmtTerm =>
        // Get the record constructor type from the context
        val constructorTyAndVal = ctx.getKnown(ToplevelV(AbsoluteRef(ctx.currentModule, recordDef.name), newTypeTerm, recordDef.uniqId, None)).get
        val recordTy = constructorTyAndVal.ty match {
          case FunctionType(_, retTy, _, _) => retTy
          case _ => Type(LevelFinite(IntegerTerm(0, None), None), None)
        }

        if (expr.telescopes.size != 1) {
          ck.reporter(
            FunctionCallArityMismatchError(
              1,
              expr.telescopes.size,
              expr
            )
          )
          return ErrorTerm(FunctionCallArityMismatchError(1, expr.telescopes.size, expr), None)
        }

        val telescopeArgs = expr.telescopes.head.args
        val args = telescopeArgs.flatMap { arg =>
          arg.expr match {
            case Tuple(values, _) => values
            case value => Vector(value)
          }
        }

        if (args.size != recordDef.fields.size) {
          ck.reporter(
            FunctionCallArityMismatchError(
              recordDef.fields.size,
              args.size,
              expr
            )
          )
          return ErrorTerm(FunctionCallArityMismatchError(recordDef.fields.size, args.size, expr), None)
        }

        // Elaborate and unify each argument with its corresponding field type
        val elaboratedArgs = args.zip(recordDef.fields).map { case (arg, field) =>
          val argTerm = elab(arg, toId(field.ty), effects)
          state.addPropagator(Unify(toId(field.ty), toId(argTerm), expr))
          argTerm
        }

        // Create the tuple type for the arguments
        val tupleType = TupleType(recordDef.fields.map(_.ty), None)
        val tupleArg = TupleTerm(elaboratedArgs, None)
        state.addPropagator(Unify(toId(tupleType), toId(tupleArg), expr))

        // Create the record constructor call term
        val recordCallTerm = RecordConstructorCallTerm(recordDef.name, Vector(tupleArg), None)

        // Unify the result type with the expected type
        state.addPropagator(Unify(ty, toId(recordTy), expr))

        recordCallTerm

      case _ =>
        defaultElabFunctionCall(expr, ty, effects)
    }
  }

  def defaultElabFunctionCall(
      expr: DesaltFunctionCall,
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      ctx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = {
    // Elaborate the function expression to get its term and type
    val functionTy = newType
    val functionTerm = elab(expr.function, functionTy, effects)

    // Only reduce if this is a type-level function
    given ReduceContext = ReduceContext()
    given Reducer = NaiveReducer
    val reducedF = functionTerm match {
      case f: Function if isTypeLevelFunction(functionTy)(using state) => Reducer.reduce(f)
      case _ => functionTerm
    }

    // Elaborate the arguments in the telescopes
    val callings = expr.telescopes.map { telescope =>
      val callingArgs = telescope.args.map { arg =>
        val argTy = newTypeTerm
        val argTerm = elab(arg.expr, argTy, effects)
        CallingArgTerm(argTerm, argTy, arg.name.map(_.name), arg.vararg, meta = None)
      }
      Calling(callingArgs, telescope.implicitly, meta = None)
    }

    // Create a placeholder for the function call term
    val functionCallTerm = newMeta

    // Create a new type variable for the function's result type
    val resultTy = newType

    // Add a propagator to unify the function type with the arguments and construct the function call term
    state.addPropagator(
      UnifyFunctionCall(
        functionTy,
        callings.toVector,
        resultTy,
        expr,
        reducedF,
        functionCallTerm
      )
    )

    // Unify the result type with the expected type
    unify(ty, resultTy, expr)

    // For type-level functions, we need to reduce the result
    val result = toTerm(functionCallTerm)
    if (isTypeLevelFunction(functionTy)(using state)) {
      Reducer.reduce(result)
    } else {
      result
    }
  }

  private def isTypeLevelFunction(ty: CellId[Term])(using s: StateAbility[Tyck]): Boolean = {
    s.readUnstable(ty) match {
      case Some(FunctionType(_, resultType, _, _)) => 
        resultType match {
          case _: Type => true
          case _ => false
        }
      case _ => false
    }
  }

  case class UnifyFunctionCall(
      functionTy: CellId[Term],
      callings: Vector[Calling],
      resultTy: CellId[Term],
      cause: Expr,
      functionTerm: Term,
      functionCallTerm: CellId[Term]
  )(using Context)
      extends Propagator[Tyck] {

    override val readingCells: Set[CellIdAny] = Set(functionTy)
    override val writingCells: Set[CellIdAny] = Set(resultTy, functionCallTerm)
    override val zonkingCells: Set[CellIdAny] = Set(resultTy, functionCallTerm)

    override def run(using state: StateAbility[Tyck], ck: Tyck): Boolean = {
      val readFunctionTy = state.readStable(functionTy)
      readFunctionTy match {
        case Some(FunctionType(telescopes, retTy, _, _)) =>
          // Unify the telescopes, handling implicit parameters
          val adjustedCallings = unifyTelescopes(telescopes, callings, cause)
          // Unify the result type
          unify(resultTy, retTy, cause)
          // Construct the function call term with adjusted callings
          val fCallTerm = FCallTerm(functionTerm, adjustedCallings, meta = None)
          state.fill(functionCallTerm, fCallTerm)
          true
        case Some(Meta(id)) =>
          // If the function type is a meta variable, delay until it is known
          state.addPropagator(
            UnifyFunctionCall(
              id,
              callings,
              resultTy,
              cause,
              functionTerm,
              functionCallTerm
            )
          )
          true
        case Some(other) =>
          // Report a function call unification error
          val argTypes = callings.flatMap(_.args.map(_.value))
          ck.reporter(FunctionCallUnificationError(other, argTypes, cause))
          true
        case None =>
          // Function type is not yet known; cannot proceed
          false
      }
    }

    // Unify expected and actual telescopes, handling implicit parameters
    def unifyTelescopes(
        expected: Vector[TelescopeTerm],
        actual: Vector[Calling],
        cause: Expr
    )(using
        state: StateAbility[Tyck],
        ck: Tyck
    ): Vector[Calling] = {
      var actualIndex = 0
      var adjustedCallings: Vector[Calling] = Vector.empty

      expected.foreach { expectedTele =>
        val hasActual = actualIndex < actual.length
        val actualTeleOpt = Option.when(hasActual)(actual(actualIndex))

        val matchesProvided = actualTeleOpt.exists(_.implicitly == expectedTele.implicitly)

        if (matchesProvided) {
          // Telescopes match; proceed to unify their arguments
          val actualTele = actualTeleOpt.get
          unifyArgs(expectedTele.args, actualTele.args, cause)
          adjustedCallings = adjustedCallings :+ actualTele
          actualIndex += 1
        } else {
          if (expectedTele.implicitly) {
            // Expected implicit telescope not provided; infer arguments
            val callingArgs = expectedTele.args.map { argTerm =>
              // For now, throw an exception
              throw new NotImplementedError(s"Implicit parameter with identifier '$argTerm' is not implemented yet.")
            }
            val calling = Calling(callingArgs, implicitly = true, meta = None)
            adjustedCallings = adjustedCallings :+ calling
            // Unify the inferred arguments
            unifyArgs(expectedTele.args, calling.args, cause)
          } else {
            // Expected explicit telescope not matched; report error
            ck.reporter(FunctionCallArityMismatchError(expected.length, actual.length, cause))
            return adjustedCallings
          }
        }
      }
      adjustedCallings
    }

    // Unify the arguments of expected and actual telescopes
    def unifyArgs(
        expectedArgs: Vector[ArgTerm],
        actualArgs: Seq[CallingArgTerm],
        cause: Expr
    )(using
        state: StateAbility[Tyck],
        ck: Tyck
    ): Unit = {
      // Check that the number of arguments matches
      if (expectedArgs.length != actualArgs.length) {
        ck.reporter(
          FunctionCallArgumentMismatchError(
            expectedArgs.length,
            actualArgs.length,
            cause
          )
        )
        return
      }

      // Unify each pair of expected and actual argument types
      expectedArgs.lazyZip(actualArgs).foreach { (expectedArg, actualArg) =>
        unify(expectedArg.ty, actualArg.ty, cause)
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using StateAbility[Tyck], Tyck): ZonkResult = {
      ZonkResult.Require(Vector(functionTy))
    }
  }

  // Placeholder for future implicit argument inference implementation
  def inferImplicitArguments(
      functionTy: CellId[Term],
      expr: DesaltFunctionCall
  )(using
      Context,
      StateAbility[Tyck],
      Tyck
  ): List[Calling] = {
    // TODO: Implement logic to infer implicit arguments based on the function type
    // For now, return an empty list as a stub
    List.empty
  }
}
