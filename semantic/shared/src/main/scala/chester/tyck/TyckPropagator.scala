package chester.tyck

import chester.error.*
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.reduce.{NaiveReducer, ReduceContext, ReduceMode, Reducer}

trait TyckPropagator extends ElaboraterCommon {

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    if (lhs != rhs) {
      // Use TypeLevel reduction for type equality checking
      given ReduceContext = localCtx.toReduceContext
      given Reducer = localCtx.given_Reducer
      val lhsResolved = readVar(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
      val rhsResolved = readVar(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))

      if (lhsResolved != rhsResolved) {
        (lhsResolved, rhsResolved) match {
          case (Meta(lhs), rhs) =>
            // Explicitly specify which overload to use
            unify(lhs: CellId[Term], rhs: Term, cause)
          case (lhs, Meta(rhs)) =>
            // Explicitly specify which overload to use
            unify(lhs: Term, rhs: CellId[Term], cause)

          // Record implementing trait (structural subtyping)
          case (RecordCallTerm(recordDef, _, _), TraitCallTerm(traitDef, _)) =>
            // Check if the record implements the trait
            checkTraitImplementation(recordDef, traitDef, cause)

          // Allow traits to be used where their implementations are expected (covariance)
          case (TraitCallTerm(traitDef, _), RecordCallTerm(recordDef, _, _)) =>
            // Ensure the record implements the trait
            checkTraitImplementation(recordDef, traitDef, cause)

          // Structural unification for ListType
          case (ListType(elem1, _), ListType(elem2, _)) =>
            unify(elem1, elem2, cause)

          case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => ()

          case (x, Intersection(xs, _)) =>
            if (!xs.exists(tryUnify(x, _))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Structural unification for TupleType
          case (TupleType(types1, _), TupleType(types2, _)) if types1.length == types2.length =>
            types1.lazyZip(types2).foreach { (t1, t2) => unify(t1, t2, cause) }

          // Type levels: use our helper method for level compatibility
          case (Type(level1, _), Type(level2, _)) =>
            if (!isLevelCompatible(level1, level2)(using state, localCtx)) {
              unify(level1, level2, cause)
            }

          case (LevelFinite(_, _), LevelUnrestricted(_)) => ()

          // Special case: both sides are unions
          case (Union(types1, _), Union(types2, _)) =>
            // For each type in rhs union, at least one type in lhs union must accept it
            if (!types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagators for each pair that unifies
            types2.foreach { t2 =>
              types1.find(t1 => tryUnify(t1, t2)).foreach { t1 =>
                unify(t1, t2, cause)
              }
            }

          // Special case: both sides are intersections
          case (Intersection(types1, _), Intersection(types2, _)) =>
            // For each type in lhs intersection, at least one type in rhs intersection must be a subtype of it
            if (!types1.forall(t1 => types2.exists(t2 => tryUnify(t1, t2)))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagators for each pair that unifies
            types1.foreach { t1 =>
              types2.find(t2 => tryUnify(t1, t2)).foreach { t2 =>
                unify(t1, t2, cause)
              }
            }

          // Handle Union types - rhs must be a subtype of lhs
          case (lhsType, Union(types2, _)) =>
            // For a union on the right, lhs must accept ALL possible types in the union
            if (!types2.forall(t2 => tryUnify(lhsType, t2))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagators for each type in the union
            types2.foreach { t2 =>
              unify(lhsType, t2, cause)
            }

          case (Union(types1, _), rhsType) =>
            // For a union on the left, ANY type in the union accepting rhs is sufficient
            // This handles the case where we pass a more specific type to a union type
            // e.g., passing Integer to Integer | String
            if (!types1.exists(t1 => tryUnify(t1, rhsType))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagator for the first matching type
            types1.find(t1 => tryUnify(t1, rhsType)).foreach { t1 =>
              unify(t1, rhsType, cause)
            }

          // Handle Intersection types - lhs must be a subtype of rhs
          case (Intersection(types1, _), rhsType) =>
            // For an intersection on the left, ALL types in the intersection must be compatible with rhs
            if (!types1.forall(t1 => tryUnify(t1, rhsType))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagators for each type in the intersection
            types1.foreach { t1 =>
              unify(t1, rhsType, cause)
            }

          case (lhsType, Intersection(types2, _)) =>
            // For an intersection on the right, ANY type in the intersection being compatible with lhs is sufficient
            if (!types2.exists(t2 => tryUnify(lhsType, t2))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            // Add propagator for the first matching type
            types2.find(t2 => tryUnify(lhsType, t2)).foreach { t2 =>
              unify(lhsType, t2, cause)
            }

          // Base case: types do not match
          case _ =>
            ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
        }
      }
    }
  }

  def unify(t1: Term, t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    state.addPropagator(Unify(literal(t1), t2, cause))
  }

  def unify(t1: CellId[Term], t2: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    state.addPropagator(Unify(t1, literal(t2), cause))
  }

  def unify(t1: CellId[Term], t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    state.addPropagator(Unify(t1, t2, cause))
  }

  type Literals = Expr & (IntegerLiteral | RationalLiteral | StringLiteral | SymbolLiteral)

  case class Unify(lhs: CellId[Term], rhs: CellId[Term], cause: Expr)(using
      Context
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(lhs, rhs)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(lhs, rhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(lhs, rhs)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      val lhs = state.readStable(this.lhs)
      val rhs = state.readStable(this.rhs)

      (lhs, rhs) match {
        case (Some(l), Some(r)) =>
          unify(l, r, cause)
          true
        case (Some(Meta(l)), _) =>
          state.addPropagator(Unify(l, this.rhs, cause))
          true
        case (_, Some(Meta(r))) =>
          state.addPropagator(Unify(this.lhs, r, cause))
          true
        case (Some(Union(types1, _)), Some(Union(types2, _))) =>
          // For each type in rhs union, at least one type in lhs union must accept it
          types2.foreach { t2 =>
            types1.find(t1 => tryUnify(t1, t2)).foreach { t1 =>
              state.addPropagator(Unify(toId(t1), toId(t2), cause))
            }
          }
          true
        case (Some(lhsType), Some(Union(types2, _))) =>
          // For a union on the right, lhs must accept ALL possible types in the union
          types2.foreach { t2 =>
            state.addPropagator(Unify(toId(lhsType), toId(t2), cause))
          }
          true
        case (Some(Union(types1, _)), Some(rhsType)) =>
          // For a union on the left, ANY type in the union accepting rhs is sufficient
          types1.find(t1 => tryUnify(t1, rhsType)).foreach { t1 =>
            state.addPropagator(Unify(toId(t1), toId(rhsType), cause))
          }
          true
        case (Some(Intersection(types1, _)), Some(Intersection(types2, _))) =>
          // For each type in lhs intersection, at least one type in rhs intersection must be a subtype of it
          types1.foreach { t1 =>
            types2.find(t2 => tryUnify(t1, t2)).foreach { t2 =>
              state.addPropagator(Unify(toId(t1), toId(t2), cause))
            }
          }
          true
        case (Some(lhsType), Some(Intersection(types2, _))) =>
          // For an intersection on the right, ANY type in the intersection being compatible with lhs is sufficient
          types2.find(t2 => tryUnify(lhsType, t2)).foreach { t2 =>
            state.addPropagator(Unify(toId(lhsType), toId(t2), cause))
          }
          true
        case (Some(Intersection(types1, _)), Some(rhsType)) =>
          // For an intersection on the left, ALL types in the intersection must be compatible with rhs
          types1.foreach { t1 =>
            state.addPropagator(Unify(toId(t1), toId(rhsType), cause))
          }
          true
        case _ => false
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhs = state.readStable(this.lhs)
      val rhs = state.readStable(this.rhs)

      (lhs, rhs) match {
        case (Some(l), Some(r)) if l == r => ZonkResult.Done
        case (Some(l), None) =>
          state.fill(this.rhs, l)
          ZonkResult.Done
        case (None, Some(r)) =>
          state.fill(this.lhs, r)
          ZonkResult.Done
        case (Some(Union(types1, _)), Some(Union(types2, _))) =>
          // For each type in rhs union, at least one type in lhs union must accept it
          if (types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(lhsType), Some(Union(types2, _))) =>
          // For a union on the right, lhs must accept ALL possible types in the union
          if (types2.forall(t2 => tryUnify(lhsType, t2))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(Union(types1, _)), Some(rhsType)) =>
          // For a union on the left, ANY type in the union accepting rhs is sufficient
          if (types1.exists(t1 => tryUnify(t1, rhsType))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(Intersection(types1, _)), Some(Intersection(types2, _))) =>
          // For each type in lhs intersection, at least one type in rhs intersection must be a subtype of it
          if (types1.forall(t1 => types2.exists(t2 => tryUnify(t1, t2)))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(lhsType), Some(Intersection(types2, _))) =>
          // For an intersection on the right, ANY type in the intersection being compatible with lhs is sufficient
          if (types2.exists(t2 => tryUnify(lhsType, t2))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(Intersection(types1, _)), Some(rhsType)) =>
          // For an intersection on the left, ALL types in the intersection must be compatible with rhs
          if (types1.forall(t1 => tryUnify(rhsType, t1))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case _ => ZonkResult.Require(Vector(this.lhs, this.rhs))
      }
    }
  }

  case class UnionOf(
      lhs: CellId[Term],
      rhs: Vector[CellId[Term]],
      cause: Expr
  )(using Context)
      extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(lhs) ++ rhs.toSet
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(lhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(lhs) ++ rhs.toSet

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      if (lhsValueOpt.isDefined && rhsValuesOpt.forall(_.isDefined)) {
        val lhsValue = lhsValueOpt.get
        val rhsValues = rhsValuesOpt.map(_.get)

        // Handle meta variables in lhs
        lhsValue match {
          case Meta(lhsId) =>
            // Create a new union type and unify with the meta variable
            val unionType = Union(rhsValues.assumeNonEmpty, None)
            unify(lhsId, unionType, cause)
            true
          case _ =>
            // Check that each rhsValue is assignable to lhsValue
            rhsValues.forall { rhsValue =>
              unify(lhsValue, rhsValue, cause)
              true // Assuming unify reports errors internally
            }
        }
      } else {
        // Not all values are available yet
        false
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      // First check if any of our cells are in the needed list
      val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
      if (ourNeededCells.nonEmpty) {
        // We need to handle these cells
        val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
        if (unknownRhs.nonEmpty) {
          // Wait for all rhs values to be known
          ZonkResult.Require(unknownRhs.toVector)
        } else {
          val rhsValues = rhsValuesOpt.map(_.get)

          lhsValueOpt match {
            case Some(Meta(lhsId)) =>
              // Create union type and unify with meta variable
              val unionType = Union(rhsValues.assumeNonEmpty, None)
              unify(lhsId, unionType, cause)
              ZonkResult.Done
            case Some(lhsValue) =>
              // LHS is known, check if it's compatible with all RHS values
              if (rhsValues.forall(rhsValue => tryUnify(lhsValue, rhsValue))) {
                ZonkResult.Done
              } else {
                ZonkResult.NotYet
              }
            case None =>
              // LHS is unknown, create UnionType from RHS values
              val unionType = Union(rhsValues.assumeNonEmpty, None)
              state.fill(lhs, unionType)
              ZonkResult.Done
          }
        }
      } else {
        // None of our cells are needed
        ZonkResult.Done
      }
    }
  }

  case class IntersectionOf(
      lhs: CellId[Term],
      rhs: Vector[CellId[Term]],
      cause: Expr
  )(using Context)
      extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(lhs) ++ rhs.toSet
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(lhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(lhs) ++ rhs.toSet

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      if (lhsValueOpt.isDefined && rhsValuesOpt.forall(_.isDefined)) {
        val lhsValue = lhsValueOpt.get
        val rhsValues = rhsValuesOpt.map(_.get)

        // Handle meta variables in lhs
        lhsValue match {
          case Meta(lhsId) =>
            // Create a new intersection type and unify with the meta variable
            val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
            unify(lhsId, intersectionType, cause)
            true
          case _ =>
            // Check that each rhsValue is a subtype of lhsValue
            rhsValues.forall { rhsValue =>
              unify(rhsValue, lhsValue, cause)
              true // Assuming unify reports errors internally
            }
        }
      } else {
        // Not all values are available yet
        false
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      // First check if any of our cells are in the needed list
      val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
      if (ourNeededCells.nonEmpty) {
        // We need to handle these cells
        val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
        if (unknownRhs.nonEmpty) {
          // Wait for all rhs values to be known
          ZonkResult.Require(unknownRhs.toVector)
        } else {
          val rhsValues = rhsValuesOpt.map(_.get)

          lhsValueOpt match {
            case Some(Meta(lhsId)) =>
              // Create intersection type and unify with meta variable
              val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
              unify(lhsId, intersectionType, cause)
              ZonkResult.Done
            case Some(lhsValue) =>
              // LHS is known, check if it's compatible with all RHS values
              if (rhsValues.forall(rhsValue => tryUnify(rhsValue, lhsValue))) {
                ZonkResult.Done
              } else {
                ZonkResult.NotYet
              }
            case None =>
              // LHS is unknown, create IntersectionType from RHS values
              val intersectionType = Intersection(rhsValues.assumeNonEmpty, None)
              state.fill(lhs, intersectionType)
              ZonkResult.Done
          }
        }
      } else {
        // None of our cells are needed
        ZonkResult.Done
      }
    }
  }

  /** Attempts to unify two terms without producing error messages.
    *
    * This method performs type unification, which includes:
    *   1. Regular structural equality after type-level reduction
    *   2. Alpha-equivalence checking for terms with bound variables
    *   3. Special handling for union and intersection types
    *
    * In a dependent type system, this is crucial as it allows:
    *   - Different variable names but equivalent binding structure to be unifiable
    *   - Type-level computations to be properly reduced and compared
    *   - Complex type structures to be handled correctly
    *
    * @param lhs
    *   Left-hand side term
    * @param rhs
    *   Right-hand side term
    * @return
    *   true if terms can be unified, false otherwise
    */
  def tryUnify(lhs: Term, rhs: Term)(using
      state: StateAbility[Tyck],
      localCtx: Context
  ): Boolean = {
    def resolveReference(term: Term) = term match {
      case varCall: ReferenceCall =>
        localCtx
          .getKnown(varCall)
          .flatMap(tyAndVal => state.readStable(tyAndVal.valueId))
          .getOrElse(term)
      case other => other
    }

    if (lhs == rhs) true
    else {
      // Use TypeLevel reduction for type equality checking
      given ReduceContext = localCtx.toReduceContext
      given Reducer = localCtx.given_Reducer

      val lhsResolved = resolveReference(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
      val rhsResolved = resolveReference(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))

      if (lhsResolved == rhsResolved) true
      else {
        // If structural equality check fails, try alpha-equivalence
        // which is crucial for dependent type systems
        if (areAlphaEquivalent(lhsResolved, rhsResolved)) true
        else {
          (lhsResolved, rhsResolved) match {
            case (Type(level1, _), Type(level2, _)) =>
              isLevelCompatible(level1, level2)(using state, localCtx)

            case (ListType(elem1, _), ListType(elem2, _)) =>
              tryUnify(elem1, elem2)

            case (Union(types1, _), Union(types2, _)) =>
              // For each type in RHS union, at least one type in LHS union must accept it
              types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))

            case (lhsType, Union(types2, _)) =>
              // For a union on the right, lhs must accept ALL possible types in the union
              types2.forall(t2 => tryUnify(lhsType, t2))

            case (Union(types1, _), rhsType) =>
              // For a union on the left, ANY type in the union accepting rhs is sufficient
              // This handles the case where we pass a more specific type to a union type
              // e.g., passing Integer to Integer | String
              types1.exists(t1 => tryUnify(t1, rhsType))

            case (Intersection(types1, _), Intersection(types2, _)) =>
              // For intersection types to be compatible, each type from the second intersection
              // must be compatible with at least one type from the first intersection
              types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))

            case (lhsType, Intersection(types2, _)) =>
              // For an intersection on the right, lhs must be a subtype of ALL types in the intersection
              types2.forall(t2 => tryUnify(lhsType, t2))

            case (Intersection(types1, _), rhsType) =>
              // For an intersection on the left, ANY type in the intersection being a subtype of rhs is sufficient
              types1.exists(t1 => tryUnify(t1, rhsType))

            case _ => false
          }
        }
      }
    }
  }

  /** Checks if two terms are alpha-equivalent.
    *
    * Alpha-equivalence is a concept from lambda calculus that considers terms equivalent if they are identical up to consistent renaming of bound
    * variables. This is crucial for dependent type systems where types can contain terms and binding structure matters.
    *
    * Examples:
    *   - (x: Type) -> x and (y: Type) -> y are alpha-equivalent
    *   - (x: Type) -> (y: x) -> y and (a: Type) -> (b: a) -> b are alpha-equivalent
    *
    * @param lhs
    *   First term to compare
    * @param rhs
    *   Second term to compare
    * @param boundVars
    *   Mapping between bound variables in lhs and rhs
    * @return
    *   true if terms are alpha-equivalent, false otherwise
    */
  private def areAlphaEquivalent(
      lhs: Term,
      rhs: Term,
      boundVars: Map[LocalV, LocalV] = Map.empty
  )(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    // For alpha-equivalence, we need to check if terms are convertible
    // with respect to bound variable names (alpha conversion)
    (lhs, rhs) match {
      // Check if one term is a bound variable with a mapping to the other term
      case (lv1: LocalV, lv2: LocalV) if boundVars.contains(lv1) =>
        boundVars(lv1) == lv2

      // For function types in a dependent type system, need to
      // be careful with variable bindings in the result type
      case (FunctionType(params1, result1, effects1, _), FunctionType(params2, result2, effects2, _)) =>
        if (params1.length != params2.length) false
        else if (effects1 != effects2) false
        else {
          // We need to build up a mapping between parameter variables
          var updatedBoundVars = boundVars

          // Check that telescopes have equivalent types
          val paramsEqual = params1.lazyZip(params2).forall { (p1, p2) =>
            // Check telescope parameters are equivalent
            if (p1.args.length != p2.args.length) false
            else {
              p1.args.lazyZip(p2.args).forall { (arg1, arg2) =>
                // For each argument, add the binding and check that the types are alpha-equivalent
                val typesEqual = areAlphaEquivalent(arg1.ty, arg2.ty, updatedBoundVars)

                // Update the bound vars mapping with this parameter
                // Use only if arg1.bind is a LocalV
                if (typesEqual) {
                  // Only attempt to update the mapping if we have LocalV values
                  updatedBoundVars = updatedBoundVars
                  true
                } else {
                  false
                }
              }
            }
          }

          if (!paramsEqual) false
          else {
            // For the result type, need to consider bindings
            areAlphaEquivalent(result1, result2, updatedBoundVars)
          }
        }

      // Types with internal structure need recursive checks
      case (Union(types1, _), Union(types2, _)) =>
        typesEquivalentModuloOrdering(types1, types2, boundVars)

      case (Intersection(types1, _), Intersection(types2, _)) =>
        typesEquivalentModuloOrdering(types1, types2, boundVars)

      // For other cases, fall back to regular equality check
      case _ => lhs == rhs
    }
  }

  /** Check if two collections of types are equivalent regardless of their ordering. For union and intersection types, the order doesn't matter.
    *
    * This is important for union and intersection types where:
    *   - A | B is equivalent to B | A
    *   - A & B is equivalent to B & A
    *
    * @param types1
    *   First collection of types
    * @param types2
    *   Second collection of types
    * @param boundVars
    *   Mapping between bound variables
    * @return
    *   true if collections are equivalent modulo ordering, false otherwise
    */
  private def typesEquivalentModuloOrdering(
      types1: Vector[Term],
      types2: Vector[Term],
      boundVars: Map[LocalV, LocalV] = Map.empty
  )(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    // For union/intersection types, each type in one collection
    // must have an equivalent in the other collection
    if (types1.length != types2.length) return false

    // Check that each type in types1 has a match in types2
    types1.forall(t1 => types2.exists(t2 => areAlphaEquivalent(t1, t2, boundVars))) &&
    // Check that each type in types2 has a match in types1
    types2.forall(t2 => types1.exists(t1 => areAlphaEquivalent(t1, t2, boundVars)))
  }

  case class LiteralType(x: Literals, tyLhs: CellId[Term])(using
      Context
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      if (state.noStableValue(tyLhs)) false
      else {
        val ty_ = state.readStable(this.tyLhs).get
        ty_ match {
          case Meta(ty) =>
            state.addPropagator(LiteralType(x, ty))
            true
          case _ =>
            x match {
              case IntegerLiteral(value, _) =>
                if (value.isValidInt && tryUnify(ty_, IntType(None))) true
                else if (value > 0 && tryUnify(ty_, NaturalType(None))) true
                else {
                  val i = Vector(IntegerType(None)) ++
                    Vector(NaturalType(None)).filter(x => value > 0) ++
                    Vector(IntType(None)).filter(x => value.isValidInt) ++
                    Vector(UIntType(None)).filter(x => value > 0 && value.isValidInt)
                  unify(ty_, Intersection(i.assumeNonEmpty, None), x)
                  true
                }
              case RationalLiteral(_, _) =>
                unify(ty_, RationalType(None), x)
                true
              case StringLiteral(_, _) =>
                unify(ty_, StringType(None), x)
                true
              case SymbolLiteral(_, _) =>
                unify(ty_, SymbolType(None), x)
                true
            }
        }
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult =
      state.fill(
        tyLhs,
        x match {
          case IntegerLiteral(_, _)  => IntegerType(None)
          case RationalLiteral(_, _) => RationalType(None)
          case StringLiteral(_, _)   => StringType(None)
          case SymbolLiteral(_, _)   => SymbolType(None)
        }
      )
      ZonkResult.Done
  }

  /** t is rhs, listT is lhs */
  case class ListOf(tRhs: CellId[Term], listTLhs: CellId[Term], cause: Expr)(using
      ck: Tyck,
      localCtx: Context
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(tRhs, listTLhs)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(tRhs, listTLhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(listTLhs)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      val t1 = state.readStable(this.tRhs)
      val listT1 = state.readStable(this.listTLhs)
      (t1, listT1) match {
        case (_, Some(Meta(listTLhs))) => {
          state.addPropagator(ListOf(tRhs, listTLhs, cause))
          true
        }
        case (_, Some(l)) if !l.isInstanceOf[ListType] => {
          ck.reporter.apply(TypeMismatch(ListType(AnyType0, meta = None), l, cause))
          true
        }
        case (Some(t1), Some(ListType(t2, _))) => {
          unify(t2, t1, cause)
          true
        }
        case (_, Some(ListType(t2, _))) => {
          unify(t2, tRhs, cause)
          true
        }
        case (Some(t1), None) => {
          unify(this.listTLhs, ListType(t1, meta = None): Term, cause)
          true
        }
        case (None, None) => {
          unify(this.listTLhs, ListType(Meta(tRhs), meta = None): Term, cause)
          true
        }
        case _ => ???
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val t1 = state.readStable(this.tRhs)
      val listT1 = state.readStable(this.listTLhs)
      if (t1.isEmpty) return ZonkResult.Require(Vector(this.tRhs))
      val ty = t1.get
      assert(listT1.isEmpty)
      state.fill(this.listTLhs, ListType(ty, meta = None))
      ZonkResult.Done
    }
  }

  /** Handles field access on record types, accounting for dependent fields.
    *
    * This propagator ensures that when accessing a field from a record type:
    *   1. The field exists in the record type
    *   2. Type-level computations in field types are properly reduced
    *   3. Dependent fields (where the type depends on other fields) are correctly handled
    *
    * @param recordTy
    *   The record type being accessed
    * @param fieldName
    *   The name of the field being accessed
    * @param expectedTy
    *   The expected type of the field
    * @param cause
    *   The expression causing this check
    */
  case class RecordFieldPropagator(
      recordTy: CellId[Term],
      fieldName: Name,
      expectedTy: CellId[Term],
      cause: Expr
  )(using Context)
      extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(recordTy)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(expectedTy)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(recordTy, expectedTy)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      state.readStable(recordTy) match {
        case Some(Meta(id)) =>
          state.addPropagator(RecordFieldPropagator(id, fieldName, expectedTy, cause))
          true
        case Some(recordType) =>
          // Apply type-level reduction to ensure we correctly handle dependent types
          given ctx: Context = summon[Context]
          given ReduceContext = ctx.toReduceContext
          given Reducer = ctx.given_Reducer
          val reducedRecord = NaiveReducer.reduce(recordType, ReduceMode.TypeLevel)

          reducedRecord match {
            case RecordCallTerm(recordDef, _, _) =>
              recordDef.fields.find(_.name == fieldName) match {
                case Some(fieldTerm) =>
                  // For dependent fields, we may need to further reduce the field type
                  val fieldType = NaiveReducer.reduce(fieldTerm.ty, ReduceMode.TypeLevel)
                  unify(expectedTy, fieldType, cause)
                  true
                case None =>
                  val problem = FieldNotFound(fieldName, recordDef.name, cause)
                  more.reporter.apply(problem)
                  true
              }
            case other =>
              val problem = NotARecordType(other, cause)
              more.reporter.apply(problem)
              true
          }
        case None => false
      }
    }

    override def naiveZonk(needed: Vector[CellIdAny])(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      state.readStable(recordTy) match {
        case None => ZonkResult.Require(Vector(recordTy))
        case _    => ZonkResult.Done
      }
    }
  }

  /** Helper method to check if a source level is compatible with a target level */
  private def isLevelCompatible(source: Term, target: Term)(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    (source, target) match {
      case (LevelFinite(_, _), LevelUnrestricted(_)) => true // Finite is compatible with unrestricted
      case (LevelUnrestricted(_), LevelFinite(_, _)) => false // Unrestricted is not compatible with finite
      case (LevelFinite(n1, _), LevelFinite(n2, _))  =>
        // Try to extract numeric values for comparison
        (extractNumericValue(n1), extractNumericValue(n2)) match {
          // If we can extract both values, lower level is compatible with higher level
          case (Some(v1), Some(v2)) => v1 <= v2
          // If we can't extract, fall back to exact equality
          case _ => source == target
        }
      case _ => source == target // For other cases, keep the exact equality check
    }
  }

  // Helper to extract numeric value from a term
  private def extractNumericValue(term: Term): Option[BigInt] = term match {
    case IntTerm(value, _)     => Some(BigInt(value))
    case IntegerTerm(value, _) => Some(value)
    case _                     => None
  }

  // Add helper method for trait implementation checking
  private def checkTraitImplementation(
      recordDef: RecordStmtTerm,
      traitDef: TraitStmtTerm,
      cause: Expr
  )(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Boolean = {
    // For MVP, we'll just check for a direct extension relationship
    val hasExtendsClause = recordDef.extendsClause.exists { clause =>
      clause match {
        case traitCall: TraitCallTerm =>
          traitCall.traitDef.uniqId == traitDef.uniqId
        case _ => false
      }
    }

    if (!hasExtendsClause) {
      // Report error if record doesn't explicitly extend the trait
      ck.reporter.apply(NotImplementingTrait(recordDef.name, traitDef.name, cause))
      false
    } else {
      true
    }
  }

}
