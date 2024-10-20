package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector

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

    // Check if the function refers to a record definition
    val functionExpr = expr.function

    val resultTerm = functionExpr match {
      case Identifier(name, _) =>
        ctx.getTypeDefinition(name) match {
          case Some(recordDef: RecordStmtTerm) =>
            // Elaborate the arguments
            val argTerms = expr.telescopes.flatMap(_.args.map { arg =>
              elab(arg.expr, newTypeTerm, effects)
            })
            val recordCallTerm = RecordConstructorCallTerm(recordDef.name, argTerms)
            // TODO: Unify the type with the expected type
            // unify(ty, recordDefType, expr)
            recordCallTerm
          case _ =>
            // Proceed with default elaboration
            defaultElabFunctionCall(expr, ty, effects)
        }
      case _ =>
        // Proceed with default elaboration for other expressions
        defaultElabFunctionCall(expr, ty, effects)
    }

    resultTerm
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

    // **No need to infer implicit arguments here**
    // We will handle implicit arguments during unification

    // Elaborate the arguments in the telescopes
    val callings = expr.telescopes.map { telescope =>
      val callingArgs = telescope.args.map { arg =>
        val argTy = newTypeTerm
        val argTerm = elab(arg.expr, argTy, effects)
        CallingArgTerm(argTerm, argTy, arg.name.map(_.name), arg.vararg)
      }
      Calling(callingArgs, telescope.implicitly)
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
        functionTerm,
        functionCallTerm
      )
    )

    // Unify the result type with the expected type
    unify(ty, resultTy, expr)

    toTerm(functionCallTerm)
  }

  case class UnifyFunctionCall(
      functionTy: CellId[Term],
      callings: Vector[Calling],
      resultTy: CellId[Term],
      cause: Expr,
      functionTerm: Term,
      functionCallTerm: CellId[Term]
  )(using localCtx: Context)
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
          val fCallTerm = FCallTerm(functionTerm, adjustedCallings)
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
        val actualTeleOpt = if (hasActual) Some(actual(actualIndex)) else None

        val matchesProvided = actualTeleOpt match {
          case Some(actualTele) => actualTele.implicitly == expectedTele.implicitly
          case None             => false
        }

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
            val calling = Calling(callingArgs, implicitly = true)
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
        actualArgs: Vector[CallingArgTerm],
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
      expectedArgs.zip(actualArgs).foreach { case (expectedArg, actualArg) =>
        unify(expectedArg.ty, actualArg.ty, cause)
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], ck: Tyck): ZonkResult = {
      ZonkResult.Require(Vector(functionTy))
    }
  }

  // Placeholder for future implicit argument inference implementation
  def inferImplicitArguments(
      functionTy: CellId[Term],
      expr: DesaltFunctionCall
  )(using
      ctx: Context,
      state: StateAbility[Tyck],
      ck: Tyck
  ): List[Calling] = {

    // TODO: Implement logic to infer implicit arguments based on the function type
    // For now, return an empty list as a stub
    List.empty
  }
}
