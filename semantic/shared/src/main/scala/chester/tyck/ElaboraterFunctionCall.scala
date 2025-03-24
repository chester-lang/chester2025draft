package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector
import chester.utils.Debug

trait ElaboraterFunctionCall { this: ElaboraterBase & ElaboraterCommon =>
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

  /** Safely fills a cell with a value, handling the case where the cell already has a value. This prevents "requirement failed" exceptions when
    * OnceCell.fill is called twice.
    */
  protected def safelyFillCell[T](
      cell: CellId[T],
      value: T,
      debugCategory: Debug.DebugCategory = Debug.DebugCategory.Tyck
  )(using
      state: StateAbility[Tyck],
      more: Tyck
  ): Unit = {
    // Check if the cell already has a value before attempting to fill it
    val existingValue = state.readUnstable(cell)
    if (existingValue.isEmpty) {
      state.fill(cell, value)
    } else if (existingValue.get != value && Debug.isEnabled(debugCategory)) {
      // Only log a warning when debug is enabled and values differ
      Debug.debugPrint(debugCategory, s"WARNING: Cell already has different value: ${existingValue.get} vs new: $value")
    }
  }
}

trait ProvideElaboraterFunctionCall extends ElaboraterFunctionCall { this: Elaborater & ElaboraterBase & ElaboraterCommon =>
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
            val recordCallTerm = RecordConstructTerm(recordDef.name, argTerms, meta = None)
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

    // Create a new effects cell for the function call
    val callEffects = newEffects

    // Elaborate the arguments in the telescopes
    val callings = expr.telescopes.map { telescope =>
      val callingArgs = telescope.args.map { arg =>
        val argTy = newTypeTerm
        val argTerm = elab(arg.expr, argTy, callEffects)
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
        functionTerm,
        functionCallTerm,
        callEffects,
        effects
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
      functionCallTerm: CellId[Term],
      callEffects: CIdOf[EffectsCell],
      outerEffects: CIdOf[EffectsCell]
  )(using Context)
      extends Propagator[Tyck] {

    override val readingCells: Set[CellIdAny] = Set(functionTy)
    override val writingCells: Set[CellIdAny] = Set(resultTy, functionCallTerm)
    override val zonkingCells: Set[CellIdAny] = Set(resultTy, functionCallTerm)

    override def run(using state: StateAbility[Tyck], ck: Tyck): Boolean = {
      import Debug.DebugCategory

      val debugTyck = Debug.isEnabled(DebugCategory.Tyck)
      if (debugTyck) {
        Debug.debugPrint(
          DebugCategory.Tyck,
          s"""UnifyFunctionCall.run: 
             |  Function term: $functionTerm
             |  Function type cell: $functionTy
             |  Result type cell: $resultTy
             |  Call effects: $callEffects
             |  Outer effects: $outerEffects""".stripMargin
        )
      }

      val readFunctionTy = state.readStable(functionTy)
      if (debugTyck) Debug.debugPrint(DebugCategory.Tyck, s"Read function type: $readFunctionTy")

      readFunctionTy match {
        case Some(FunctionType(telescopes, retTy, functionEffects, _)) =>
          if (debugTyck) Debug.debugPrint(DebugCategory.Tyck, s"Matched FunctionType with telescopes: $telescopes, retTy: $retTy, effects: $functionEffects")

          // Unify the telescopes, handling implicit parameters
          val adjustedCallings = unifyTelescopes(telescopes, callings, cause)
          if (debugTyck) Debug.debugPrint(DebugCategory.Tyck, s"Adjusted callings: $adjustedCallings")

          // Unify the result type
          unify(resultTy, retTy, cause)

          // Propagate effects from the function call to the outer effects
          propagateEffects(functionEffects, outerEffects, cause)

          // Construct the function call term with adjusted callings
          val fCallTerm = FCallTerm(functionTerm, adjustedCallings, meta = None)
          if (debugTyck) Debug.debugPrint(DebugCategory.Tyck, s"Created function call term: $fCallTerm")

          // Use the helper method to safely fill the cell
          safelyFillCell(functionCallTerm, fCallTerm, DebugCategory.Tyck)

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
              functionCallTerm,
              callEffects,
              outerEffects
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

    // Propagate effects from function call to outer effects
    private def propagateEffects(
        functionEffects: Term,
        outerEffects: CIdOf[EffectsCell],
        cause: Expr
    )(using state: StateAbility[Tyck], ck: Tyck): Unit = {
      functionEffects match {
        case Effects(effects, _) =>
          // Add each effect from the function to the outer effects
          effects.foreach { (_, effect) =>
            val effectsCell = state.readCell(outerEffects).asInstanceOf[EffectsCell]
            effectsCell.requireEffect(effect)
          }
        case Meta(id) =>
          // If effects are a meta variable, create a propagator to handle them once resolved
          state.addPropagator(PropagateEffects(id, outerEffects, cause))
        case _ => // do nothing for other cases
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
      var continueProcessing = true

      expected.takeWhile(_ => continueProcessing).foreach { expectedTele =>
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
            continueProcessing = false
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

  // Helper case class for effect propagation
  case class PropagateEffects(
      effectsCell: CellId[Term],
      callerEffects: CIdOf[EffectsCell],
      expr: Expr
  ) extends Propagator[Tyck] {
    override def readingCells: Set[CIdOf[Cell[?]]] = Set(effectsCell)
    override def writingCells: Set[CIdOf[Cell[?]]] = Set(callerEffects)
    override def zonkingCells: Set[CIdOf[Cell[?]]] = Set()

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      state.readStable(effectsCell) match {
        case Some(Effects(effects, _)) =>
          // Add each effect from the callee to the caller's effects
          effects.foreach { (_, effect) =>
            val effectsCell = state.readCell(callerEffects).asInstanceOf[EffectsCell]
            effectsCell.requireEffect(effect)
          }
          true
        case Some(Meta(_)) => false // Effects not yet resolved
        case _ => true // No effects to propagate
      }
    }

    override def naiveZonk(
        needed: Vector[CIdOf[Cell[?]]]
    )(using StateAbility[Tyck], Tyck): ZonkResult = {
      ZonkResult.Require(Vector(effectsCell))
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
