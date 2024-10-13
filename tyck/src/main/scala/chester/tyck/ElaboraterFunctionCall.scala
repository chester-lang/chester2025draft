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

    // Elaborate the function expression to get its term and type
    val functionTy = newType
    val functionTerm = elab(expr.function, functionTy, effects)

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

    override val readingCells: Set[CellId[?]] = Set(functionTy)
    override val writingCells: Set[CellId[?]] = Set(resultTy, functionCallTerm)
    override val zonkingCells: Set[CellId[?]] = Set(resultTy, functionCallTerm)

    override def run(using state: StateAbility[Tyck], ck: Tyck): Boolean = {
      val readFunctionTy = state.readStable(functionTy)
      readFunctionTy match {
        case Some(FunctionType(telescopes, retTy, _, _)) =>
          // Unify the arguments with the function's parameters
          unifyTelescopes(telescopes, callings, cause)
          // Unify the result type
          unify(resultTy, retTy, cause)
          // Now we can construct the function call term
          val fCallTerm = FCallTerm(functionTerm, callings)
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
          // Report specific function call unification error
          val argTypes = callings.flatMap(_.args.map(_.value))
          ck.reporter(FunctionCallUnificationError(other, argTypes, cause))
          true
        case None =>
          // Function type is not yet known, cannot proceed
          false
      }
    }

    def unifyTelescopes(
        expected: Vector[TelescopeTerm],
        actual: Vector[Calling],
        cause: Expr
    )(using
        state: StateAbility[Tyck],
        ck: Tyck
    ): Unit = {
      // Check that the number of telescopes matches
      if (expected.length != actual.length) {
        actual.flatMap(_.args.map(_.value))
        FunctionType(expected, newTypeTerm)
        ck.reporter(
          FunctionCallArityMismatchError(expected.length, actual.length, cause)
        )
        return
      }

      expected.zip(actual).foreach { case (expectedTele, actualCalling) =>
        unifyArgs(expectedTele.args, actualCalling.args, cause)
      }
    }

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

      expectedArgs.zip(actualArgs).foreach { case (expectedArg, actualArg) =>
        // Unify argument types
        unify(expectedArg.ty, actualArg.ty, cause)
      }
    }

    override def naiveZonk(
        needed: Vector[CellId[?]]
    )(using state: StateAbility[Tyck], ck: Tyck): ZonkResult = {
      ZonkResult.Require(Vector(functionTy))
    }
  }
}
