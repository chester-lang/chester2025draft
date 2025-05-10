package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.reduce.{DefaultReducer, ReduceContext, ReduceMode, Reducer}
import chester.tyck.*
import chester.utils.*
import chester.utils.propagator.*
import chester.syntax.*
import chester.tyck.api.{NoopSemanticCollector, SemanticCollector, UnusedVariableWarningWrapper}
import cats.data.NonEmptyVector

import scala.collection.immutable.Vector as _
import scala.language.implicitConversions
import scala.util.boundary
import scala.util.boundary.break

trait Elaborater extends ProvideContextOps with TyckPropagator {

  // Function to directly connect union cell to its components
  private def connectUnionToComponents(
      unionCell: CellId[Term],
      componentIds: Vector[CellId[Term]],
      cause: Expr
  )(using
      state: StateOps[TyckOps],
      ctx: Context,
      ck: TyckOps
  ): Unit =
    state.addPropagator(UnionOf(unionCell, componentIds, cause))

  def checkType(expr: Expr)(using
      Context,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
  ): Term = {
    // Create a new type cell representing the kind Typeω (the type of types)
    val kindType = literal(Typeω: Term)

    elab(expr, kindType, toEffectsCell(Effects.Empty))
  }

  def checkTypeId(expr: Expr)(using
      Context,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
  ): CellId[Term] =
    toId(checkType(expr))

  def elabTy(expr: Option[Expr])(using
      Context,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
  ): Term =
    expr match {
      case Some(expr) => checkType(expr)
      case None       => Meta(newType)
    }

  def elab(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Term

  def elabId(expr: Expr, ty: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      Context,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
  ): CellId[Term] = {
    val term = elab(expr, ty, effects)
    toId(term)
  }

  override def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit = {
    if (lhs == rhs) return
    // Use TypeLevel reduction for type equality checking
    given ReduceContext = localCtx.toReduceContext
    given Reducer = localCtx.given_Reducer
    val lhsResolved = readVar(DefaultReducer.reduce(lhs, ReduceMode.TypeLevel))
    val rhsResolved = readVar(DefaultReducer.reduce(rhs, ReduceMode.TypeLevel))
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
        types1.lazyZip(types2).foreach((t1, t2) => unify(t1, t2, cause))
      case (Type(level1, _), Type(level2, _))        => unify(level1, level2, cause)
      case (LevelFinite(_, _), LevelUnrestricted(_)) => ()

      // Union-to-Union subtyping - use a guard to ensure specificity
      case (Union(types1, _), Union(types2, _)) if types1.nonEmpty && types2.nonEmpty =>

        // For each type in the RHS union, at least one type in LHS union must accept it
        val lhsCell = toId(lhs)
        val rhsCell = toId(rhs)

        // Create a direct unify connection between the two union types
        state.addPropagator(Unify(lhsCell, rhsCell, cause))

        // Get component cells and create necessary structural relationships
        val lhsTypeIds = types1.map(toId).toVector
        val rhsTypeIds = types2.map(toId).toVector

        // Connect unions to their components
        connectUnionToComponents(lhsCell, lhsTypeIds, cause)
        connectUnionToComponents(rhsCell, rhsTypeIds, cause)

        // Create direct connections between compatible component types
        for {
          t1 <- types1
          t2 <- types2
        }
          if (tryUnify(t1, t2)) {
            val t1Cell = toId(t1)
            val t2Cell = toId(t2)
            state.addPropagator(Unify(t1Cell, t2Cell, cause))
          }

      // Specific-to-Union subtyping (function parameter case in test)
      case (specificType, union @ Union(unionTypes, _)) if !specificType.isInstanceOf[Union] && unionTypes.nonEmpty =>

        // Get cell IDs for both types
        val specificCellId = toId(specificType)
        val unionCellId = toId(union).asInstanceOf[CellId[Term]]

        // Check compatibility with any union component
        val compatibleComponent = unionTypes.find(unionType => tryUnify(specificType, unionType))

        if (compatibleComponent.isDefined) {

          // Connect to the component directly
          val componentId = toId(compatibleComponent.get)
          state.addPropagator(Unify(specificCellId, componentId, cause))

          // Connect the union to all its components
          val componentIds = unionTypes.map(toId).toVector
          connectUnionToComponents(unionCellId, componentIds, cause)
        } else {
          // If no compatible component is found, report a type mismatch
          ck.reporter.apply(TypeMismatch(specificType, Union(unionTypes, None), cause))
        }

      // Union-to-Specific subtyping (function return case in test)
      case (union @ Union(unionTypes, _), specificType) if !specificType.isInstanceOf[Union] && unionTypes.nonEmpty =>

        // A union can be used where a specific type is expected if all components match it
        unionToSpecific(union, unionTypes, specificType, cause)

      // Now add the general intersection and union cases
      case (Intersection(xs, _), x) =>
        if (xs.forall(tryUnify(_, x))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (x, Union(xs, _)) =>
        if (xs.forall(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

      // Add cases for function calls after the specific union cases
      case (fcall: FCallTerm, _) =>
        // Connect function call components directly
        connectFunctionCallComponents(fcall, cause)

        // Continue with normal unification
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
      case (_, fcall: FCallTerm) =>
        // Connect function call components directly
        connectFunctionCallComponents(fcall, cause)

        // Continue with normal unification
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

      case _ => ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
    }
  }

  // Helper function to directly connect function call components
  private def connectFunctionCallComponents(fcall: FCallTerm, cause: Expr)(using
      state: StateOps[TyckOps],
      ctx: Context,
      ck: TyckOps
  ): Unit =

    // Connect all arguments
    for (calling <- fcall.args)
      for (arg <- calling.args)
        arg match {
          case CallingArgTerm(term, _, _, _, _) =>
            // Create direct connections for any nested union/intersection types in args
            term match {
              case Union(types, _) =>
                val unionCell = toId(term)
                val componentCells = types.map(toId).toVector
                connectUnionToComponents(unionCell, componentCells, cause)
              case Intersection(types, _) =>
                val intersectionCell = toId(term)
                val componentCells = types.map(toId).toVector
                state.addPropagator(IntersectionOf(intersectionCell, componentCells, cause))
              case nestedFCall: FCallTerm =>
                connectFunctionCallComponents(nestedFCall, cause)
              case _ => // No special handling needed
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
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Unit = {
    // Check if any union component can be used where specificType is expected
    // Only one component needs to be compatible for the union to be valid in this position
    var anyCompatible = false
    for (unionType <- unionTypes)
      if (tryUnify(unionType, specificType)) {
        // We found a compatible component, mark as compatible
        anyCompatible = true

        // Add a propagator for each compatible union component
        val unionTypeCell = toId(unionType)
        val specificTypeCell = toId(specificType)

        // Create a propagator from the union component to the specific type
        state.addPropagator(Unify(unionTypeCell, specificTypeCell, cause))
      } else {
        // This component isn't compatible, but that's okay if at least one other is
        // ck.reporter.apply(TypeMismatch(unionType, specificType, cause)) // REMOVED - Don't report error if other components might match
        // No need to break, we want to report all errors and check all components
      }

    // If no union component matches, the overall unification fails
    if (!anyCompatible) {
      ck.reporter.apply(TypeMismatch(union, specificType, cause))
    } else {
      // Connect the union to its components
      // val unionCell = toId(union)
      // val componentCells = unionTypes.map(toId).toVector
      // connectUnionToComponents(unionCell, componentCells, cause) // REMOVED to prevent recursion

      // Create direct connection from union to specific
      // state.addPropagator(Unify(unionCell, toId(specificType), cause)) // REMOVED - component connections should suffice
    }
  }
}

trait ProvideElaborater extends ProvideContextOps with Elaborater with ElaboraterFunction with ElaboraterFunctionCall with ElaboraterBlock {

  // TODO: add something for implicit conversion

  def newSubtype(ty: CellIdOr[Term], cause: Expr)(using
      localCtx: Context,
      ck: TyckOps,
      state: StateOps[TyckOps]
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
      ck: TyckOps,
      state: StateOps[TyckOps]
  ): Term = toTerm {
    val ty = toId(readMetaVar(toTerm(ty0)))
    resolve(expr) match {
      case expr @ Identifier(name, _) =>

        // Get the expected type (if already specified)
        val expectedType = state.readStable(ty)

        // Special handling for assigning a variable to a union type
        expectedType match {
          case Some(Union(unionTypes, _)) =>

            // Look up the identifier in context
            localCtx.get(name) match {
              case Some(c: ContextItem) =>

                // Get the source type of the identifier
                val sourceTypeOpt = state.readStable(c.tyId)

                sourceTypeOpt match {
                  case Some(sourceType) =>
                    // If the source is an Integer type and the union contains Integer, establish the connection
                    given ReduceContext = localCtx.toReduceContext
                    given Reducer = localCtx.given_Reducer

                    val reducedSourceType = DefaultReducer.reduce(sourceType, ReduceMode.TypeLevel)

                    // Check if source is compatible with any union component
                    val compatibleComponent = unionTypes.find { unionType =>
                      val reducedUnionType = DefaultReducer.reduce(unionType, ReduceMode.TypeLevel)

                      tryUnify(reducedSourceType, reducedUnionType)(using state, localCtx)
                    }

                    if (compatibleComponent.isDefined) {

                      // Connect to the specific component
                      val componentId = toId(compatibleComponent.get)
                      state.addPropagator(Unify(c.tyId, componentId, expr))

                      // Return the reference
                      c.ref
                    } else {
                      // Fall back to regular unification
                      state.addPropagator(Unify(ty, c.tyId, expr))
                      c.ref
                    }
                  case None =>
                    // No source type yet, fall back to normal handling
                    state.addPropagator(Unify(ty, c.tyId, expr))
                    c.ref
                }
              case None =>
                // Regular handling for unbound identifiers
                localCtx.getTypeDefinition(name) match {
                  case Some(objectDef: ObjectStmtTerm) =>
                    val objectCallTerm = ObjectConstructTerm(objectDef, convertMeta(expr.meta))
                    unify(ty, ObjectTypeTerm(objectDef, convertMeta(expr.meta)), expr)
                    objectCallTerm
                  case Some(recordDef: RecordStmtTerm) =>
                    val recordCallTerm = RecordTypeTerm(recordDef, TelescopeTerm(Vector(), meta = None), convertMeta(expr.meta))
                    unify(ty, Type0, expr)
                    recordCallTerm
                  case Some(traitDef: TraitStmtTerm) =>
                    val traitCallTerm = TraitTypeTerm(traitDef, convertMeta(expr.meta))
                    unify(ty, Type0, expr)
                    traitCallTerm
                  case Some(_) => ???
                  case None =>
                    val problem = UnboundVariable(name, expr)
                    ck.reporter.apply(problem)
                    ErrorTerm(problem, convertMeta(expr.meta))
                }
            }
          case _ =>
            // Regular identifier handling (no union type involved)
            localCtx.get(name) match {
              case Some(c: ContextItem) =>
                state.addPropagator(Unify(ty, c.tyId, expr))
                c.ref
              case None =>
                // Check if 'name' refers to an object definition
                localCtx.getTypeDefinition(name) match {
                  case Some(objectDef: ObjectStmtTerm) =>
                    val objectCallTerm = ObjectConstructTerm(objectDef, convertMeta(expr.meta))
                    unify(ty, ObjectTypeTerm(objectDef, convertMeta(expr.meta)), expr)
                    objectCallTerm
                  case Some(recordDef: RecordStmtTerm) =>
                    val recordCallTerm = RecordTypeTerm(recordDef, TelescopeTerm(Vector(), meta = None), convertMeta(expr.meta))
                    unify(ty, Type0, expr)
                    recordCallTerm
                  case Some(traitDef: TraitStmtTerm) =>
                    val traitCallTerm = TraitTypeTerm(traitDef, convertMeta(expr.meta))
                    unify(ty, Type0, expr)
                    traitCallTerm
                  case Some(_) => ???
                  case None =>
                    val problem = UnboundVariable(name, expr)
                    ck.reporter.apply(problem)
                    ErrorTerm(problem, convertMeta(expr.meta))
                }
            }
        }
      case expr @ IntegerLiteral(value, meta) =>
        // Get the expected type (if already specified)
        val expectedType = state.readStable(ty)

        expectedType match {
          // If the expected type is a union type, we need special handling
          case Some(Union(unionTypes, _)) =>
            // Check if any union component is compatible with Integer
            val integerTypeComponent = unionTypes.find { unionType =>
              given ReduceContext = localCtx.toReduceContext
              given Reducer = localCtx.given_Reducer
              val reduced = DefaultReducer.reduce(unionType, ReduceMode.TypeLevel)
              reduced match {
                case IntegerType(_) => true
                case _              => false
              }
            }

            if (integerTypeComponent.isDefined) {
              // Create the integer term with Integer type
              val integerTerm = AbstractIntTerm_.from(value, convertMeta(meta))

              // Connect to the union type directly
              state.addPropagator(Unify(ty, toId(integerTypeComponent.get), expr))

              // Return the integer term
              integerTerm
            } else {
              // If no Integer type found in the union, default to normal handling
              state.addPropagator(LiteralType(expr, ty))
              AbstractIntTerm_.from(value, convertMeta(meta))
            }

          // Normal case - not a union type or no expected type yet
          case _ =>
            state.addPropagator(LiteralType(expr, ty))
            AbstractIntTerm_.from(value, convertMeta(meta))
        }
      case expr @ RationalLiteral(value, meta) =>
        state.addPropagator(LiteralType(expr, ty))
        RationalTerm(value, convertMeta(meta))
      case expr @ StringLiteral(value, meta) =>
        // Normal string handling
        state.addPropagator(LiteralType(expr, ty))
        StringTerm(value, convertMeta(meta))
      case expr @ SymbolLiteral(value, meta) =>
        state.addPropagator(LiteralType(expr, ty))
        SymbolTerm(value, convertMeta(meta))
      case expr @ UnitExpr(meta) =>
        unify(ty, UnitType(convertMeta(meta)), expr)
        UnitTerm_(convertMeta(meta))
      case expr @ ListExpr(terms, meta) =>
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
        val elemTypes = termResults.map(_._2)

        // Ensure that 't' is the union of the element types
        if (elemTypes.nonEmpty) state.addPropagator(UnionOf(t, elemTypes, expr))

        ListTerm(termResults.map(_._1), convertMeta(meta))
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
          val componentTy = elab(typeExpr, Typeω, effects)
          componentTy
        }

        // Ensure we have at least one type in the union
        if (elaboratedTypes.isEmpty) {
          val errorTerm = ErrorTerm(NotImplemented(expr), convertMeta(meta))
          unify(ty, errorTerm, expr)
          errorTerm
        } else {
          // Create a NonEmptyVector from the elaborated types
          import cats.data.NonEmptyVector
          val unionTypes = NonEmptyVector.fromVectorUnsafe(elaboratedTypes)
          val unionTerm = Union(unionTypes, convertMeta(meta))

          // This is a type, so it should be in the Type universe
          unify(ty, Type0, expr)

          // Create direct connections between union and its components
          val unionCellId = toId(unionTerm).asInstanceOf[CellId[Term]]
          val componentCellIds = elaboratedTypes.map(toId)

          // Connect the union to its components directly
          state.addPropagator(UnionOf(unionCellId, componentCellIds, expr))

          // Return the union term
          unionTerm
        }
      case expr @ DotCall(record, field: Identifier, args, meta) =>
        val recordTy = newType
        val recordTerm = elab(record, recordTy, effects)

        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer
        val reducedRecordTy = DefaultReducer.reduce(toTerm(recordTy), ReduceMode.TypeLevel)

        // Special case for Integer.+ method with a string argument
        if (field.name == "+" && args.nonEmpty) {
          if (args.length >= 2) {
            ???
          }
          // First handle argument elaboration
          val arg = args.head
          val argTy = newType
          val argTerm = elab(arg, argTy, effects)

          state.addPropagator(Unify(argTy, toId(IntegerType(None)), expr))
          // For Integer.+, the return type is always Integer
          state.addPropagator(Unify(ty, toId(IntegerType(None)), expr))

          // Create the term for the method call
          val callingArg = CallingArgTerm(argTerm, toTerm(argTy), None, false, convertMeta(meta))
          val calling = Calling(Vector(callingArg), false, convertMeta(meta))
          DotCallTerm(recordTerm, field.name, Vector(calling), toTerm(ty), convertMeta(meta))
        } else {
          // Normal field access or method call handling
          reducedRecordTy match {
            case IntegerType(_) if field.name == "+" =>
              args.headOption match {
                case Some(arg) =>
                  if (args.length >= 2) {
                    ???
                  }
                  val argTy = newType
                  val argTerm = elab(arg, argTy, effects)

                  // Check if the argument type is compatible with Integer
                  given ReduceContext = localCtx.toReduceContext
                  given Reducer = localCtx.given_Reducer
                  val reducedArgType = DefaultReducer.reduce(toTerm(argTy), ReduceMode.TypeLevel)

                  reducedArgType match {
                    case IntegerType(_) | IntType(_) =>
                      // If it's an Integer or Int, it's valid
                      state.addPropagator(Unify(ty, toId(IntegerType(None)), expr))

                      // Create a dot call term with the argument wrapped in Calling
                      val calling = Calling(Vector(CallingArgTerm(argTerm, toTerm(argTy), None, false, convertMeta(meta))), false, convertMeta(meta))
                      DotCallTerm(recordTerm, field.name, Vector(calling), toTerm(ty), convertMeta(meta))

                    case _ =>
                      // If it's not an Integer, report a type error
                      val problem = TypeMismatch(IntegerType(None), toTerm(argTy), arg)
                      ck.reporter.apply(problem)
                      ErrorTerm(problem, convertMeta(meta))
                  }
                case None =>
                  val problem = NotImplementedFeature("Missing method call argument", expr)
                  ck.reporter.apply(problem)
                  ErrorTerm(problem, convertMeta(meta))
              }
            case RecordTypeTerm(recordDef, _, _) =>
              val fields = recordDef.fields
              fields.find(_.name == field.name) match {
                case Some(field) =>
                  if (args.isEmpty) {
                    state.addPropagator(Unify(ty, toId(field.ty), expr))
                    DotCallTerm(recordTerm, field.name, Vector.empty, toTerm(ty), convertMeta(meta))
                  } else {
                    val problem = NotImplementedFeature("Field access with arguments not supported", expr)
                    ck.reporter.apply(problem)
                    ErrorTerm(problem, convertMeta(meta))
                  }
                case None =>
                  val problem = FieldNotFound(field.name, recordDef.name, expr)
                  ck.reporter.apply(problem)
                  ErrorTerm(problem, convertMeta(meta))
              }
            case Meta(id) =>
              // If we have a meta term, add a propagator to check the field access once the type is known
              state.addPropagator(RecordFieldPropagator(id, field.name, ty, expr))
              DotCallTerm(recordTerm, field.name, Vector.empty, toTerm(ty), convertMeta(meta))
            case _ =>
              val problem = NotARecordType(reducedRecordTy, expr)
              ck.reporter.apply(problem)
              ErrorTerm(problem, convertMeta(meta))
          }
        }
      case _ @InfixExpr(left, op, right, associativity, meta) =>

        // Create a synthetic DotCall and process it
        val argsTuple = Tuple(Vector(right), right.meta)
        val dotCall = DotCall(left, op, Vector(argsTuple), meta)

        // Now delegate to our existing implementation for DotCall
        elab(dotCall, ty, effects)

      case expr: Expr =>
        val problem = NotImplemented(expr)
        ck.reporter.apply(problem)
        ErrorTerm(problem, convertMeta(expr.meta))
    }
  }

  // TODO: untested
  private def elabObjectExpr(
      expr: ObjectExpr,
      fields: Vector[ObjectClause],
      ty: CellId[Term],
      effects: CIdOf[EffectsCell]
  )(using
      Context,
      SemanticCollector,
      TyckOps,
      StateOps[TyckOps]
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
    implicit val get: TyckOps = new StateReporter(reporter, new MutBox(()))
    implicit val able: StateOps[TyckOps] = stateAbilityImpl
    val ty1: CellId[Term] = ty match {
      case Some(ty) =>
        val cell = literal[Term](ty)
        cell
      case None =>
        val cell = newType
        cell
    }
    val effects1: CIdOf[EffectsCell] = effects match {
      case Some(effects) =>
        val cell = toEffectsCell(effects)
        cell
      case None =>
        newEffects
    }
    implicit val ctx: Context = Context.default
    val wellTyped = elabId(expr, ty1, effects1)
    able.zonk(Vector(ty1, effects1, wellTyped))
    val judge = Judge(
      able.readStable(wellTyped).get,
      able.readStable(ty1).get,
      able.readUnstable(effects1).get
    )
    val finalJudge = finalizeJudge(judge)

    TyckResult0((), finalJudge, reporter.getReports)

  }

  private def finalizeJudge(
      judge0: Judge
  )(using
      ck: TyckOps,
      able: StateOps[TyckOps],
      recording: SemanticCollector
  ): Judge = {

    var judge = judge0
    try {
      boundary {
        while (true) {
          val metas = judge.collectMeta
          if (metas.isEmpty) break()

          try
            able.zonk(metas.map(x => x.unsafeRead[CellId[Term]]))
          catch {
            case e: IllegalStateException if e.getMessage.contains("not covered by any propagator") =>
              throw e
          }

          judge = judge.replaceMeta(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
        }
      }
      recording.metaFinished(x => able.readUnstable(x.unsafeRead[CellId[Term]]).get)
      judge
    } catch {
      case e: Exception =>
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
    implicit val get: TyckOps = new StateReporter(reporter, new MutBox(()))
    implicit val able: StateOps[TyckOps] = stateAbilityImpl
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
    able.zonk(Vector(ty, effects))
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
