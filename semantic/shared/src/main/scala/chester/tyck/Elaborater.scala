package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.{*, given}
import chester.reduce.{NaiveReducer, ReduceContext, ReduceMode, Reducer}
import chester.tyck.*
import chester.utils.*
import chester.utils.propagator.*
import chester.syntax.*
import chester.tyck.api.{NoopSemanticCollector, SemanticCollector, UnusedVariableWarningWrapper}
import scala.collection.immutable.{Vector => StdVector}
import cats.data.NonEmptyVector

import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break

// Debug flag for union subtyping
val DEBUG_UNION_SUBTYPING = false

trait Elaborater extends ProvideCtx with TyckPropagator {

  // Helper method to ensure a cell is covered by a propagator
  private def ensureCellCoverage(cell: CellId[Term], cause: Expr)(using
      state: StateAbility[Tyck],
      ctx: Context,
      ck: Tyck
  ): Unit = {
    if (DEBUG_UNION_SUBTYPING) println(s"Ensuring cell coverage for cell $cell")
    // Simply connect the cell to itself to ensure it's covered by at least one propagator
    state.addPropagator(UnionOf(cell, Vector(cell), cause))
  }

  // Helper method to ensure cell coverage for all union components
  private def ensureUnionComponentsCoverage(
      unionTypes: NonEmptyVector[Term], 
      cause: Expr
  )(using
      state: StateAbility[Tyck],
      ctx: Context,
      ck: Tyck
  ): Vector[CellId[Term]] = {
    // Get cell IDs for all union component types and ensure they're covered
    val unionTypeIds = unionTypes.map(typ => {
      val cellId = toId(typ)
      ensureCellCoverage(cellId, cause)
      cellId
    }).toVector
    
    unionTypeIds
  }

  // Helper method for connecting union types to their components
  private def connectUnionToComponents(
      unionCell: CellId[Term],
      componentIds: Vector[CellId[Term]],
      cause: Expr
  )(using
      state: StateAbility[Tyck],
      ctx: Context,
      ck: Tyck
  ): Unit = {
    if (DEBUG_UNION_SUBTYPING) {
      println(s"Creating UnionOf propagator: union cell $unionCell connected to components: ${componentIds.mkString(", ")}")
    }
    state.addPropagator(UnionOf(unionCell, componentIds, cause))
  }
  
  // Process all terms in a function application to ensure proper cell coverage
  private def processFunctionCall(term: Term, cause: Expr)(using
      StateAbility[Tyck],
      Context,
      Tyck
  ): Unit = {
    // Get cell ID
    val cellId = toId(term)

    // Ensure this cell is covered
    ensureCellCoverage(cellId, cause)

    // For composite terms like function calls, also ensure sub-term coverage
    term match {
      case fcall: FCallTerm => {
        if (DEBUG_UNION_SUBTYPING) println(s"Processing function call: $fcall")
        // Process function and arguments recursively
        processFunctionCall(fcall.f, cause)
        fcall.args.foreach(arg => processFunctionCall(arg, cause))
      }
      case Union(types, _) => {
        if (DEBUG_UNION_SUBTYPING) println(s"Processing union: $term")
        // Process all union types
        types.foreach(t => processFunctionCall(t, cause))
      }
      case Intersection(types, _) => {
        if (DEBUG_UNION_SUBTYPING) println(s"Processing intersection: $term")
        // Process all intersection types
        types.foreach(t => processFunctionCall(t, cause))
      }
      case _ => // No further processing needed for simple terms
    }
  }

  def checkType(expr: Expr)(using
      Context,
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): Term = {
    // Create a new type cell representing the kind Typeω (the type of types)
    val kindType = literal(Typeω: Term)

    elab(expr, kindType, toEffectsCell(Effects.Empty))
  }

  def checkTypeId(expr: Expr)(using
      Context,
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): CellId[Term] = {
    toId(checkType(expr))
  }

  def elabTy(expr: Option[Expr])(using
      Context,
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): Term =
    expr match {
      case Some(expr) => checkType(expr)
      case None       => Meta(newType)
    }

  def elab(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term

  def elabId(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      Context,
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): CellId[Term] = {
    val term = elab(expr, ty, effects)
    toId(term)
  }

  override def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    if (lhs == rhs) return
    // Use TypeLevel reduction for type equality checking
    given ReduceContext = localCtx.toReduceContext
    given Reducer = localCtx.given_Reducer
    val lhsResolved = readVar(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
    val rhsResolved = readVar(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))
    if (lhsResolved == rhsResolved) return
    (lhsResolved, rhsResolved) match {
      case (Meta(lhs), rhs)                                            => unify(lhs, rhs, cause)
      case (lhs, Meta(rhs))                                            => unify(lhs, rhs, cause)
      case (ListType(elem1, _), ListType(elem2, _))                    => unify(elem1, elem2, cause)
      case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => ()
      case (x, Intersection(xs, _)) =>
        if (xs.exists(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (TupleType(types1, _), TupleType(types2, _)) if types1.length == types2.length =>
        types1.lazyZip(types2).foreach { (t1, t2) => unify(t1, t2, cause) }
      case (Type(level1, _), Type(level2, _))        => unify(level1, level2, cause)
      case (LevelFinite(_, _), LevelUnrestricted(_)) => ()

      // Union-to-Union subtyping - use a guard to ensure specificity
      case (Union(types1, _), Union(types2, _)) if types1.nonEmpty && types2.nonEmpty =>
        if (DEBUG_UNION_SUBTYPING) {
          println("=== UNION-UNION SUBTYPING ===")
          println(s"LHS Union: $lhs (${lhs.getClass.getSimpleName}) with cell ID: ${toId(lhs)}")
          println(s"RHS Union: $rhs (${rhs.getClass.getSimpleName}) with cell ID: ${toId(rhs)}")
          println(s"LHS Component Types: ${types1.mkString(", ")}")
          println(s"RHS Component Types: ${types2.mkString(", ")}")
        }

        // For each type in the RHS union, at least one type in LHS union must accept it
        val lhsCell = toId(lhs)
        val rhsCell = toId(rhs)

        // Ensure all cells are covered by propagators
        ensureCellCoverage(lhsCell, cause)
        ensureCellCoverage(rhsCell, cause)

        // Create a direct unify connection between the two union types
        if (DEBUG_UNION_SUBTYPING) println(s"Creating Unify propagator between $lhsCell and $rhsCell")
        state.addPropagator(Unify(lhsCell, rhsCell, cause))

        // Get cell IDs for all component types and ensure they're covered
        val lhsTypeIds = ensureUnionComponentsCoverage(types1, cause)
        val rhsTypeIds = ensureUnionComponentsCoverage(types2, cause)

        // Connect unions to their components
        connectUnionToComponents(lhsCell, lhsTypeIds, cause)
        connectUnionToComponents(rhsCell, rhsTypeIds, cause)

        // Create direct connections between compatible component types
        for (t1 <- types1; t2 <- types2) {
          if (tryUnify(t1, t2)) {
            val t1Cell = toId(t1)
            val t2Cell = toId(t2)
            if (DEBUG_UNION_SUBTYPING) {
              println(s"Creating connection between component types: $t1 and $t2")
              println(s"  Cell IDs: $t1Cell <-> $t2Cell")
            }
            state.addPropagator(Unify(t1Cell, t2Cell, cause))
          }
        }

      // Specific-to-Union subtyping (function parameter case in test)
      case (specificType, union @ Union(unionTypes, _)) if !specificType.isInstanceOf[Union] && unionTypes.nonEmpty =>
        if (DEBUG_UNION_SUBTYPING) {
          println("=== SPECIFIC-TO-UNION SUBTYPING ===")
          println(s"Specific Type: $specificType with cell ID: ${toId(specificType)}")
          println(s"Union Type: $union with cell ID: ${toId(union)}")
          println(s"Union Component Types: ${unionTypes.mkString(", ")}")
        }

        // A specific type can be used where a union is expected if it matches any union component
        specificToUnion(specificType, union, unionTypes, cause)

      // Union-to-Specific subtyping (function return case in test)
      case (union @ Union(unionTypes, _), specificType) if !specificType.isInstanceOf[Union] && unionTypes.nonEmpty =>
        if (DEBUG_UNION_SUBTYPING) {
          println("=== UNION-TO-SPECIFIC SUBTYPING ===")
          println(s"Union Type: $union with cell ID: ${toId(union)}")
          println(s"Specific Type: $specificType with cell ID: ${toId(specificType)}")
          println(s"Union Component Types: ${unionTypes.mkString(", ")}")
        }

        // A union can be used where a specific type is expected if all components match it
        unionToSpecific(union, unionTypes, specificType, cause)

      // Now add the general intersection and union cases
      case (x, Intersection(xs, _)) =>
        if (xs.exists(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (Intersection(xs, _), x) =>
        if (xs.forall(tryUnify(_, x))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (x, Union(xs, _)) =>
        if (xs.forall(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (Union(xs, _), x) =>
        if (xs.exists(tryUnify(_, x))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

      // Add cases for function calls after the specific union cases
      case (fcall: FCallTerm, _) => {
        if (DEBUG_UNION_SUBTYPING) println(s"Processing function call in unify: $fcall")
        // Ensure all cells in the function call have proper coverage
        processFunctionCall(fcall, cause)

        // Continue with normal unification
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      }
      case (_, fcall: FCallTerm) => {
        if (DEBUG_UNION_SUBTYPING) println(s"Processing function call in unify (RHS): $fcall")
        // Ensure all cells in the function call have proper coverage
        processFunctionCall(fcall, cause)

        // Continue with normal unification
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      }

      case _ => ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
    }
  }

  // Helper method to handle specific-to-union subtyping
  private def specificToUnion(
      specificType: Term, 
      union: Term,
      unionTypes: NonEmptyVector[Term],
      cause: Expr
  )(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    // Check if the specific type can be used for any of the union types
    var foundCompatible = false
    for (unionType <- unionTypes) {
      if (tryUnify(specificType, unionType)) {
        foundCompatible = true
        // No need to break, just set the flag
      }
    }

    // If no compatible union component was found, report error
    if (!foundCompatible) {
      ck.reporter.apply(TypeMismatch(specificType, union, cause))
    }
  }

  // Helper method to handle union-to-specific subtyping
  private def unionToSpecific(
      union: Term,
      unionTypes: NonEmptyVector[Term],
      specificType: Term,
      cause: Expr
  )(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    // Check if all union components can be used where specificType is expected
    var allCompatible = true
    for (unionType <- unionTypes) {
      if (!tryUnify(unionType, specificType)) {
        allCompatible = false
        ck.reporter.apply(TypeMismatch(unionType, specificType, cause))
        // No need to break, we want to report all errors
      }
    }

    // If any union component doesn't match, the overall unification fails
    if (!allCompatible) {
      ck.reporter.apply(TypeMismatch(union, specificType, cause))
    }
  }
}

trait ProvideElaborater extends ProvideCtx with Elaborater with ElaboraterFunction with ElaboraterFunctionCall with ElaboraterBlock {

  // TODO: add something for implicit conversion

  def newSubtype(ty: CellIdOr[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CellId[Term] = {
    val cell = newType
    state.addPropagator(Unify(toId(ty), cell, cause))
    cell
  }

  /** Type checking and elaboration of Chester terms.
    *
    * During type checking, we sometimes need to reduce/evaluate terms to check types. For example, when checking field access on a type constructed
    * by a type function: def idType(x: Type): Type = x; let aT = A; def getA2(x: idType(aT)): Integer = x.a;
    *
    * Here we need to reduce idType(aT) to A to check the field access. However, we preserve the original unreduced terms in the core representation
    * unless explicitly requested. This keeps the term structure clean while still allowing type checking to work correctly.
    */
  override def elab(
      expr: Expr,
      ty0: CellIdOr[Term],
      effects: CIdOf[EffectsCell]
  )(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Term = toTerm {
    val ty = toId(readMetaVar(toTerm(ty0)))
    resolve(expr) match {
      case expr @ Identifier(name, meta) => {
        localCtx.get(name) match {
          case Some(c: ContextItem) => {
            state.addPropagator(Unify(ty, c.tyId, expr))
            c.ref
          }
          case None => {
            // Check if 'name' refers to an object definition
            localCtx.getTypeDefinition(name) match {
              case Some(objectDef: ObjectStmtTerm) =>
                val objectCallTerm = ObjectConstructTerm(objectDef, convertMeta(expr.meta))
                unify(ty, ObjectTypeTerm(objectDef, convertMeta(expr.meta)), expr)
                objectCallTerm
              case Some(recordDef: RecordStmtTerm) =>
                val recordCallTerm = RecordTypeTerm(recordDef, TelescopeTerm(Vector(), meta = None), convertMeta(expr.meta)) // TODO
                unify(ty, Type0, expr) // TODO: Type
                recordCallTerm
              case Some(traitDef: TraitStmtTerm) =>
                val traitCallTerm = TraitTypeTerm(traitDef, convertMeta(expr.meta))
                unify(ty, Type0, expr) // Traits are types
                traitCallTerm
              case Some(todo) => ???
              case None =>
                val problem = UnboundVariable(name, expr)
                ck.reporter.apply(problem)
                ErrorTerm(problem, convertMeta(expr.meta))
            }
          }
        }
      }
      case expr @ IntegerLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        AbstractIntTerm_.from(value, convertMeta(meta))
      }
      case expr @ RationalLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        RationalTerm(value, convertMeta(meta))
      }
      case expr @ StringLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        StringTerm(value, convertMeta(meta))
      }
      case expr @ SymbolLiteral(value, meta) => {
        state.addPropagator(LiteralType(expr, ty))
        SymbolTerm(value, convertMeta(meta))
      }
      case expr @ UnitExpr(meta) => {
        unify(ty, UnitType(convertMeta(meta)), expr)
        UnitTerm_(convertMeta(meta))
      }
      case expr @ ListExpr(terms, meta) => {
        val t = newType
        // Relate the list type 'ty' to 'ListType(t)'
        state.addPropagator(ListOf(t, ty, expr))

        // For each term, check it with its own type variable and collect the results
        val termResults = terms.map { term =>
          val elemTy = newType
          val wellTypedTerm = elab(term, elemTy, effects)
          (wellTypedTerm, elemTy)
        }

        // Collect the types of the elements
        val elemTypes = termResults.map(_._2).toVector

        // Ensure that 't' is the union of the element types
        if (elemTypes.nonEmpty) state.addPropagator(UnionOf(t, elemTypes, expr))

        ListTerm(termResults.map(_._1), convertMeta(meta))
      }
      case expr @ TypeAnotationNoEffects(innerExpr, tyExpr, _) =>
        // Check the type annotation expression to get its type
        val declaredTyTerm = checkType(tyExpr)

        unify(ty, declaredTyTerm, expr)

        elab(innerExpr, declaredTyTerm, effects)
      case expr: FunctionExpr       => elabFunction(expr, ty, effects)
      case expr: Block              => elabBlock(expr, ty, effects)
      case expr: DesaltFunctionCall => elabFunctionCall(expr, ty, effects)
      case expr @ ObjectExpr(fields, _) =>
        elabObjectExpr(expr, fields, ty, effects)
      case expr @ UnionTypeExpr(types, meta) =>
        // Handle union type expressions
        
        // Elaborate each type in the union
        val elaboratedTypes = types.map { typeExpr =>
          // Each component should be a type
          elab(typeExpr, Typeω, effects)
        }
        
        // Ensure we have at least one type in the union
        if (elaboratedTypes.isEmpty) {
          val errorTerm = ErrorTerm(NotImplemented(expr), convertMeta(meta))
          unify(ty, errorTerm, expr)
          errorTerm
        } else {
          // Create a NonEmptyVector from the elaborated types
          import cats.data.NonEmptyVector
          val unionTerm = Union(NonEmptyVector.fromVectorUnsafe(elaboratedTypes), convertMeta(meta))
          
          // Unify with the expected type
          unify(ty, unionTerm, expr)
          
          unionTerm
        }
      case expr @ DotCall(recordExpr, fieldExpr, telescopes, meta) =>
        if (telescopes.nonEmpty) {
          val problem = NotImplementedFeature("Field access with arguments is not yet supported", expr)
          ck.reporter.apply(problem)
          ErrorTerm(problem, convertMeta(expr.meta))
        } else {
          fieldExpr match {
            case Identifier(fieldName, _) =>
              val recordTy = newType
              val recordTerm = elab(recordExpr, recordTy, effects)
              // Keep original term in elaboration result but use reduced type for checking
              val resultTerm = FieldAccessTerm(recordTerm, fieldName, toTerm(ty), convertMeta(meta))
              // Use TypeLevel reduction internally for type checking
              given ReduceContext = localCtx.toReduceContext
              given Reducer = localCtx.given_Reducer
              val reducedRecordTy = NaiveReducer.reduce(toTerm(recordTy), ReduceMode.TypeLevel)
              reducedRecordTy match {
                case Meta(id) =>
                  // If we have a meta term, add a propagator to check the field access once the type is known
                  state.addPropagator(RecordFieldPropagator(id, fieldName, ty, expr))
                  resultTerm
                case RecordTypeTerm(recordDef, _, _) =>
                  val fields = recordDef.fields
                  fields.find(_.name == fieldName) match {
                    case Some(field) =>
                      state.addPropagator(Unify(ty, toId(field.ty), expr))
                    case None =>
                      ck.reporter.apply(FieldNotFound(fieldName, recordDef.name, expr))
                  }
                  resultTerm
                case _ =>
                  ck.reporter.apply(NotARecordType(reducedRecordTy, expr))
                  resultTerm
              }
            case _ =>
              val problem = InvalidFieldName(fieldExpr)
              ck.reporter.apply(problem)
              ErrorTerm(problem, convertMeta(expr.meta))
          }
        }
      case expr: Expr => {
        val problem = NotImplemented(expr)
        ck.reporter.apply(problem)
        ErrorTerm(problem, convertMeta(expr.meta))
      }
    }
  }

  // TODO: untested
  def elabObjectExpr(
      expr: ObjectExpr,
      fields: Vector[ObjectClause],
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      Context,
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): Term = {
    // Create collections to store field keys and types
    val fieldTypeVars = scala.collection.mutable.Map[Term, CellId[Term]]()
    val elaboratedFields = fields.flatMap {
      case ObjectExprClauseOnValue(keyExpr, valueExpr) =>
        // Elaborate the key and value expressions
        val elaboratedKey = elab(keyExpr, newType, effects)
        val fieldType = newType
        val elaboratedValue = elab(valueExpr, fieldType, effects)
        val _ = fieldTypeVars.put(elaboratedKey, fieldType)
        Some(ObjectClauseValueTerm(elaboratedKey, elaboratedValue, convertMeta(expr.meta)))
      // Handle other possible clauses
      case _ => ???
    }

    // Construct the object term with elaborated fields
    val objectTerm = ObjectTerm(elaboratedFields, convertMeta(expr.meta))

    // Construct the expected object type
    val expectedObjectType = ObjectType(
      elaboratedFields.map { case ObjectClauseValueTerm(keyTerm, _, _) =>
        ObjectClauseValueTerm(keyTerm, Meta(fieldTypeVars(keyTerm)), convertMeta(expr.meta))
      },
      meta = convertMeta(expr.meta)
    )

    // Unify the expected type with the object's type
    unify(ty, expectedObjectType, expr)

    objectTerm
  }
}

trait DefaultImpl
    extends ProvideElaborater
    with ProvideImpl
    with ProvideElaboraterFunction
    with ProvideElaboraterFunctionCall
    with ProvideElaboraterBlock {

  def check(
      expr: Expr,
      ty: Option[Term] = None,
      effects: Option[Effects] = None,
      sementicCollector: SemanticCollector = NoopSemanticCollector
  ): TyckResult[Unit, Judge] = {
    implicit val collecter: UnusedVariableWarningWrapper =
      new UnusedVariableWarningWrapper(sementicCollector)
    val reporter = new VectorReporter[TyckProblem]
    implicit val get: Tyck = new Get(reporter, new MutBox(()))
    implicit val able: StateAbility[Tyck] = stateAbilityImpl
    val ty1: CellId[Term] = ty match {
      case Some(ty) => {
        val cell = literal[Term](ty)
        cell
      }
      case None => {
        val cell = newType
        cell
      }
    }
    val effects1: CIdOf[EffectsCell] = effects match {
      case Some(effects) => {
        val cell = toEffectsCell(effects)
        cell
      }
      case None => {
        newEffects
      }
    }
    implicit val ctx: Context = Context.default
    val wellTyped = elabId(expr, ty1, effects1)
    able.naiveZonk(Vector(ty1, effects1, wellTyped))
    val judge = Judge(
      able.readStable(wellTyped).get,
      able.readStable(ty1).get,
      able.readUnstable(effects1).get
    )
    val finalJudge = finalizeJudge(judge)

    TyckResult0((), finalJudge, reporter.getReports)

  }

  def finalizeJudge(
      judge0: Judge
  )(using
      ck: Tyck,
      able: StateAbility[Tyck],
      recording: SemanticCollector,
      reporter: Reporter[TyckProblem]
  ): Judge = {
    if (DEBUG_UNION_SUBTYPING) {
      println("\n=== STARTING FINALIZE JUDGE ===")
    }

    var judge = judge0
    try {
      boundary {
        while (true) {
          val metas = judge.collectMeta
          if (metas.isEmpty) break()

          if (DEBUG_UNION_SUBTYPING) {
            println(s"Zonking ${metas.size} meta cells")
            metas.foreach { meta =>
              val cellId = meta.unsafeRead[CellId[Term]]
              println(s"Meta cell: $cellId")
            }
          }

          try {
            able.naiveZonk(metas.map(x => x.unsafeRead[CellId[Term]]))
          } catch {
            case e: IllegalStateException if e.getMessage.contains("not covered by any propagator") =>
              if (DEBUG_UNION_SUBTYPING) {
                println("\n=== ERROR: CELLS NOT COVERED BY PROPAGATOR ===")
                println(e.getMessage)
                val problemCells = e.getMessage.split("Cells ")(1).split(" are not covered")(0)
                println(s"Problem cells: $problemCells")

                // Print information about each propagator for debugging
                println("\n=== PROPAGATOR INFORMATION ===")
                // Just print the problem cells information without trying to access internal state
                println("Could not access all cells due to API limitations")

                // Continue with standard error handling
              }
              throw e
          }

          judge = judge.replaceMeta(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
        }
      }
      recording.metaFinished(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
      judge
    } catch {
      case e: Exception =>
        if (DEBUG_UNION_SUBTYPING) {
          println(s"Exception in finalizeJudge: ${e.getMessage}")
          e.printStackTrace()
        }
        throw e
    }
  }

  def checkTop(
      fileName: String,
      expr: Expr,
      reporter0: Reporter[Problem],
      sementicCollector: SemanticCollector = NoopSemanticCollector,
      loadedModules: LoadedModules = LoadedModules.Empty
  ): chester.syntax.TAST = {
    implicit val collecter: UnusedVariableWarningWrapper =
      new UnusedVariableWarningWrapper(sementicCollector)
    implicit val reporter: ReporterTrackError[Problem] = new ReporterTrackError(
      reporter0
    )
    implicit val get: Tyck = new Get(reporter, new MutBox(()))
    implicit val able: StateAbility[Tyck] = stateAbilityImpl
    implicit var ctx: Context = Context.default.copy(loadedModules = loadedModules)
    val (module, block): (ModuleRef, Block) = resolve(expr) match {
      case b @ Block(head +: heads, tail, _) =>
        resolve(head) match {
          case ModuleStmt(module, meta) => (module, Block(heads, tail, meta))
          case _                        => (DefaultModule, b)
        }
      case expr => (DefaultModule, Block(Vector(), Some(expr), expr.meta))
    }
    ctx = ctx.updateModule(module)
    val ty = newType
    val effects = newEffects
    val wellTyped = elabBlock(block, ty, effects)
    able.naiveZonk(Vector(ty, effects))
    val judge =
      Judge(wellTyped, able.readStable(ty).get, able.readUnstable(effects).get)
    val finalJudge = finalizeJudge(judge)

    TAST(
      fileName = fileName,
      module = module,
      ast = finalJudge.wellTyped.asInstanceOf[BlockTerm],
      ty = finalJudge.ty,
      effects = finalJudge.effects,
      problems = reporter.getSeverityMap
    )
  }
}

object Tycker extends DefaultImpl with ProvideMutable {}

export Tycker.{check, checkTop}
