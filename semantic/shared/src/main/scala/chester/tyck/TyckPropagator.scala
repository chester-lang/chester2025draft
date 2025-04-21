package chester.tyck

import chester.error.*
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.reduce.{DefaultReducer, ReduceContext, ReduceMode, Reducer}
import cats.data.NonEmptyVector
import chester.utils.Debug.DebugCategory.*
import chester.i18n.*

import scala.language.implicitConversions
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec

trait TyckPropagator extends ElaboraterCommon with Alpha {

  // Helper method to handle common zonk setup for cells with left-hand side and right-hand side values
  private def setupZonk[T](
      lhs: CellId[T],
      rhs: Vector[CellId[T]],
      needed: Vector[CellIdAny]
  )(using state: StateAbility[Tyck]): Either[ZonkResult, (Option[T], Vector[T])] = {
    val lhsValueOpt = state.readStable(lhs)
    val rhsValuesOpt = rhs.map(state.readStable)

    // First check if any of our cells are in the needed list
    val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
    if (ourNeededCells.isEmpty) {
      return Left(ZonkResult.Done) // None of our cells are needed
    }

    // Check if we're waiting for rhs values
    val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
    if (unknownRhs.nonEmpty) {
      return Left(ZonkResult.Require(unknownRhs))
    }

    // Get all rhs values - we know they're all defined at this point
    val rhsValues = rhsValuesOpt.collect { case Some(value) => value }

    // Return values - include lhsValueOpt which might be None
    Right((lhsValueOpt, rhsValues))
  }

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    def addUnificationPropagator(lhsId: CellId[Term], rhsId: CellId[Term]): Unit =
      state.addPropagator(Unify(lhsId, rhsId, cause))

    if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying $lhs with $rhs")

    // Handle meta variables
    (toTerm(lhs), toTerm(rhs)) match {
      case (Meta(cellId), rhs) =>
        addUnificationPropagator(cellId, toId(rhs))
      case (lhs, Meta(cellId)) =>
        addUnificationPropagator(toId(lhs), cellId)
      case (lhs, rhs) if lhs != rhs =>
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        val lhsResolved = DefaultReducer.reduce(lhs, ReduceMode.TypeLevel)
        val rhsResolved = DefaultReducer.reduce(rhs, ReduceMode.TypeLevel)
        if (lhsResolved == rhsResolved) return

        (lhsResolved, rhsResolved) match {
          // Handle Union types - rhs must be a subtype of lhs
          case (lhsType, Union(types2, _)) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying lhsType $lhsType with union type ${types2.mkString(", ")}")
            // For a union on the right (like Integer | String),
            // the left side type (like Integer) just needs to match ONE component
            val lhsTypeId = toId(lhsType)

            // Find any compatible union component
            val compatibleComponent = types2.find(t2 => tryUnify(lhsType, t2))

            if (compatibleComponent.isDefined) {
              // Create a propagator connecting lhs to the matching component
              val compatibleId = toId(compatibleComponent.get)
              // Use the direct unification propagator
              addUnificationPropagator(lhsTypeId, compatibleId)
            } else {
              // No compatible components
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          case (Union(types1, _), rhsType) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying union type ${types1.mkString(", ")} with rhsType $rhsType")
            // For a union on the left, ANY type in the union must be compatible with rhs
            // We need to check if at least one component is compatible
            val rhsTypeId = toId(rhsType)
            
            val anyCompatible = types1.exists(t1 => tryUnify(t1, rhsType))

            if (anyCompatible) {
              // At least one component is compatible, create propagators for that component
              types1.find(t1 => tryUnify(t1, rhsType)).foreach { compatibleComponent =>
                val compatibleId = toId(compatibleComponent)
                // Use the direct unification propagator
                addUnificationPropagator(compatibleId, rhsTypeId)
              }
            } else {
              // No compatible components
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Record implementing trait (structural subtyping)
          case (RecordTypeTerm(recordDef, _, _), TraitTypeTerm(traitDef, _)) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying record type ${recordDef.name} with trait type ${traitDef.name}")
            if (!checkTraitImplementation(recordDef, traitDef, cause)) {
              if (Debug.isEnabled(TraitMatching))
                Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Record ${recordDef.name} does not implement trait ${traitDef.name}")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Record type implementing trait type (structural subtyping)
          case (lhsType @ RecordStmtTerm(name, _, fields, _, extendsClause, _), rhsType @ TraitStmtTerm(traitName, _, _, _, _)) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying record type $name with trait type $traitName")
            if (!checkTraitImplementation(lhsType, rhsType, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Record $name does not implement trait $traitName")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Trait extending trait (structural subtyping)
          case (TraitTypeTerm(childTraitDef, _), TraitTypeTerm(parentTraitDef, _)) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying child trait ${childTraitDef.name} with parent trait ${parentTraitDef.name}")
            if (!checkTraitExtends(childTraitDef, parentTraitDef, cause)) {
              if (Debug.isEnabled(TraitMatching))
                Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Trait ${childTraitDef.name} does not extend trait ${parentTraitDef.name}")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Handle Intersection types
          case (Intersection(types1, _), rhsType) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying intersection type ${types1.mkString(", ")} with rhsType $rhsType")
            if (!types1.exists(t1 => tryUnify(t1, rhsType))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          case (lhsType, Intersection(types2, _)) =>
            if (Debug.isEnabled(TraitMatching))
              Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying lhsType $lhsType with intersection type ${types2.mkString(", ")}")
            if (!types2.forall(t2 => tryUnify(lhsType, t2))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // For other cases, add a direct unification propagator
          case (lhsType, rhsType) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Unifying lhsType $lhsType with rhsType $rhsType")
            // If terms are not identical after reduction, add a propagator
            val lhsId = toId(lhsType)
            val rhsId = toId(rhsType)
            addUnificationPropagator(lhsId, rhsId)
        }
      case _ => // Terms are already equal, nothing to do
    }
  }

  def unify(t1: Term, t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit =
    state.addPropagator(Unify(literal(t1), t2, cause))

  def unify(t1: CellId[Term], t2: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit =
    state.addPropagator(Unify(t1, literal(t2), cause))

  def unify(t1: CellId[Term], t2: CellId[Term], cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit =
    state.addPropagator(Unify(t1, t2, cause))

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
        case _ => false
      }
    }

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // Only process if one of our cells is needed
      if (!needed.contains(this.lhs) && !needed.contains(this.rhs)) {
        return ZonkResult.Done
      }
      
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
          // For union-to-union, handle properly
          handleUnionUnion(types1, types2)
        case (Some(lhsType), Some(Union(types2, _))) =>
          // For specific-to-union, handle properly
          handleSpecificUnion(lhsType, types2)
        case (Some(Union(types1, _)), Some(rhsType)) =>
          // For union-to-specific, handle properly
          handleUnionSpecific(types1, rhsType)
        case (Some(Intersection(types1, _)), Some(Intersection(types2, _))) =>
          // For intersection-to-intersection
          if (types1.forall(t1 => types2.exists(t2 => tryUnify(t1, t2)))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(lhsType), Some(Intersection(types2, _))) =>
          // For specific-to-intersection
          if (types2.exists(t2 => tryUnify(lhsType, t2))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(Intersection(types1, _)), Some(rhsType)) =>
          // For intersection-to-specific
          if (types1.forall(t1 => tryUnify(t1, rhsType))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case _ => 
          // Need both values to continue
          ZonkResult.Require(Vector(this.lhs, this.rhs))
      }
    }
    
    // Handle union-to-union case properly
    private def handleUnionUnion(
      types1: NonEmptyVector[Term],
      types2: NonEmptyVector[Term]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // For each type in RHS union, at least one type in LHS union must accept it
      if (unionUnionCompatible(types1, types2)) {
        // Create proper unification between component types directly
        types2.foreach { t2 =>
          // Find compatible type in types1
          types1.find(t1 => tryUnify(t1, t2)).foreach { compatibleType =>
            // Add direct unification between these compatible types
            state.addPropagator(Unify(toId(compatibleType), toId(t2), cause))
          }
        }
        ZonkResult.Done
      } else {
        ZonkResult.NotYet
      }
    }
    
    // Handle specific-to-union case properly
    private def handleSpecificUnion(
      lhsType: Term, 
      types2: NonEmptyVector[Term]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // For a specific type to be compatible with a union type,
      // the specific type must be compatible with at least one of the union components
      if (specificUnionCompatible(lhsType, types2)) {
        // Find each compatible union component and connect directly
        types2.withFilter(unionType => tryUnify(lhsType, unionType)).foreach { compatibleType =>
          // Create direct link between specific type and compatible component
          state.addPropagator(Unify(toId(lhsType), toId(compatibleType), cause))
        }
        ZonkResult.Done
      } else {
        ZonkResult.NotYet
      }
    }
    
    // Handle union-to-specific case properly
    private def handleUnionSpecific(
      types1: NonEmptyVector[Term],
      rhsType: Term
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // For a union type to be compatible with a specific type,
      // at least one component must be compatible
      if (unionSpecificCompatible(types1, rhsType)) {
        // Connect each compatible component directly
        types1.withFilter(t1 => tryUnify(t1, rhsType)).foreach { compatibleType =>
          state.addPropagator(Unify(toId(compatibleType), toId(rhsType), cause))
        }
        ZonkResult.Done
      } else {
        ZonkResult.NotYet
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

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult =
      setupZonk(lhs, rhs, needed) match {
        case Left(result) => result
        case Right((lhsValueOpt, rhsValues)) =>
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

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult =
      setupZonk(lhs, rhs, needed) match {
        case Left(result) => result
        case Right((lhsValueOpt, rhsValues)) =>
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
    // Recursion counter for debugging
    new AtomicInteger(0)

    def tryUnifyInternal(lhs: Term, rhs: Term, depth: Int): Boolean = {
      val indent = " " * depth
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG] Trying to unify: $lhs with $rhs")

      // Helper function to fully resolve references in terms
      @tailrec
      def fullyResolveReference(term: Term): Term = {
        val resolved = term match {
          case varCall: ReferenceCall =>
            localCtx
              .getKnown(varCall)
              .flatMap(tyAndVal => state.readStable(tyAndVal.valueId))
              .getOrElse(term)
          case _ => term
        }

        // Continue resolving if we got a new reference
        if (resolved != term && resolved.isInstanceOf[ReferenceCall]) {
          fullyResolveReference(resolved)
        } else {
          resolved
        }
      }

      if (lhs == rhs) {
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Terms are equal, returning true")
        true
      } else {
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        // Fully resolve references for deeper resolution
        val lhsResolved = fullyResolveReference(DefaultReducer.reduce(lhs, ReduceMode.TypeLevel))
        val rhsResolved = fullyResolveReference(DefaultReducer.reduce(rhs, ReduceMode.TypeLevel))

        if (Debug.isEnabled(UnionMatching))
          Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   After reduction and resolution: $lhsResolved with $rhsResolved")

        // Special cases for integer literals
        (lhsResolved, rhsResolved) match {
          // Direct matching for integer values (like 42) with Union types containing Integer
          case (v: AbstractIntTerm, Union(types, _)) =>
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Checking if union contains Integer for AbstractIntTerm $v")
            val hasIntegerType = types.exists {
              case IntegerType(_) => true
              case t =>
                val reduced = DefaultReducer.reduce(t, ReduceMode.TypeLevel)
                reduced == IntegerType(None)
            }
            if (hasIntegerType) {
              if (Debug.isEnabled(UnionMatching))
                Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Union contains Integer, AbstractIntTerm is compatible with union")
              return true
            }

          // The reverse - Union containing Integer is compatible with AbstractIntTerm
          case (Union(types, _), v: AbstractIntTerm) =>
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Checking if union contains Integer for AbstractIntTerm $v")
            val hasIntegerType = types.exists {
              case IntegerType(_) => true
              case t =>
                val reduced = DefaultReducer.reduce(t, ReduceMode.TypeLevel)
                reduced == IntegerType(None)
            }
            if (hasIntegerType) {
              if (Debug.isEnabled(UnionMatching))
                Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Union contains Integer, union is compatible with AbstractIntTerm")
              return true
            }

          // Handle the case where a numeric literal needs to match with an Integer type
          case (_: IntTerm, IntegerType(_)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Integer literal matches IntegerType: true")
            return true
          case (_: IntegerTerm, IntegerType(_)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Integer literal matches IntegerType: true")
            return true
          // The inverse
          case (IntegerType(_), _: IntTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   IntegerType matches integer literal: true")
            return true
          case (IntegerType(_), _: IntegerTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   IntegerType matches integer literal: true")
            return true

          case _ => // Continue with normal processing
        }

        if (lhsResolved == rhsResolved) {
          if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Reduced terms are equal, returning true")
          true
        } else {
          // If structural equality check fails, try alpha-equivalence
          // which is crucial for dependent type systems
          if (areAlphaEquivalent(lhsResolved, rhsResolved)) {
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Terms are alpha-equivalent, returning true")
            true
          } else {
            (lhsResolved, rhsResolved) match {
              case (Type(level1, _), Type(level2, _)) =>
                val result = isLevelCompatible(level1, level2)(using state, localCtx)
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Type level check: $level1 compatible with $level2? $result")
                result

              case (ListType(elem1, _), ListType(elem2, _)) =>
                val result = tryUnifyInternal(elem1, elem2, depth + 1)
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   List element check: $result")
                result

              case (lhsType, Union(types2, _)) =>
                // For a specific type and a union type, check if the specific type
                // is compatible with at least one union component
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(
                    UnionMatching,
                    t"$indent[UNIFY DEBUG]   Checking specific type against union: $lhsType with ${types2.mkString(", ")}"
                  )
                val result = types2.exists(t2 => tryUnifyInternal(lhsType, t2, depth + 1))
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Specific-to-union check: $result")
                result

              case (Union(types1, _), rhsType) =>
                // For a union type and a specific type, check if at least one union component
                // is compatible with the specific type (NOT all components need to be compatible)
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(
                    UnionMatching,
                    t"$indent[UNIFY DEBUG]   Checking union type against specific: ${types1.mkString(", ")} with $rhsType"
                  )
                val result = types1.exists(t1 => tryUnifyInternal(t1, rhsType, depth + 1))
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   Union-to-specific check: $result")
                result

              case _ =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"$indent[UNIFY DEBUG]   No match found, returning false")
                false
            }
          }
        }
      }
    }

    // Start the recursion
    val result = tryUnifyInternal(lhs, rhs, 0)
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"[UNIFY DEBUG] Final result for unifying $lhs with $rhs: $result")
    result
  }

  // Helper method to get the appropriate type for a literal expression
  private def getLiteralType(x: Literals): Term = x match {
    case IntegerLiteral(_, _)  => IntegerType(None)
    case RationalLiteral(_, _) => RationalType(None)
    case StringLiteral(_, _)   => StringType(None)
    case SymbolLiteral(_, _)   => SymbolType(None)
  }

  case class LiteralType(x: Literals, tyLhs: CellId[Term])(using
      Context
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean =
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
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(UnionMatching, t"[LITERAL DEBUG] Processing integer literal $value with expected type $ty_")
                // Try to handle union types
                ty_ match {
                  case Union(types, _) =>
                    // Check if any of the union types is compatible with this integer literal
                    if (Debug.isEnabled(UnionMatching))
                      Debug.debugPrint(UnionMatching, t"[LITERAL DEBUG] Checking integer literal $value with union type $ty_")
                    val compatibleTypes = types.filter {
                      case IntegerType(_) => true
                      case unionType =>
                        val reduced =
                          DefaultReducer.reduce(unionType, ReduceMode.TypeLevel)(using summon[Context].toReduceContext, summon[Context].given_Reducer)
                        reduced match {
                          case IntegerType(_) => true
                          case _              => false
                        }
                    }

                    if (compatibleTypes.nonEmpty) {
                      if (Debug.isEnabled(UnionMatching))
                        Debug.debugPrint(UnionMatching, t"[LITERAL DEBUG] Found compatible types in union: ${compatibleTypes.mkString(", ")}")
                      // Found at least one compatible type in the union
                      true
                    } else {
                      if (Debug.isEnabled(UnionMatching))
                        Debug.debugPrint(UnionMatching, t"[LITERAL DEBUG] No compatible types found in union for integer literal $value")
                      more.reporter.apply(TypeMismatch(IntegerType(None), ty_, x))
                      true
                    }
                  // Handle normal integer literal case
                  case _ =>
                    if (value.isValidInt && tryUnify(ty_, IntType(None))) true
                    else if (value > 0 && tryUnify(ty_, NaturalType(None))) true
                    else if (tryUnify(ty_, IntegerType(None))) true
                    else {
                      val i = Vector(IntegerType(None)) ++
                        Vector(NaturalType(None)).filter(x => value > 0) ++
                        Vector(IntType(None)).filter(x => value.isValidInt) ++
                        Vector(UIntType(None)).filter(x => value > 0 && value.isValidInt)
                      unify(ty_, Intersection(i.assumeNonEmpty, None), x)
                      true
                    }
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

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // First, check if we already have a value for this cell
      val existingValue = state.readStable(tyLhs)
      if (existingValue.isDefined) {
        existingValue.get match {
          case Union(types, _) =>
            // If we have a union type, we need to make sure the literal is compatible with at least one component
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, t"[LITERAL ZONK DEBUG] Handling union type during zonk: $existingValue")
            val literalType = getLiteralType(x)

            // Find compatible types
            val compatibleTypes = types.filter(unionType => tryUnify(literalType, unionType)(using state, summon[Context]))

            if (compatibleTypes.nonEmpty) {
              if (Debug.isEnabled(UnionMatching))
                Debug.debugPrint(UnionMatching, t"[LITERAL ZONK DEBUG] Found compatible union components: ${compatibleTypes.mkString(", ")}")
              ZonkResult.Done // Leave the union type as is
            } else {
              if (Debug.isEnabled(UnionMatching))
                Debug.debugPrint(UnionMatching, t"[LITERAL ZONK DEBUG] No compatible union components found for literal type $literalType")
              state.fill(
                tyLhs,
                getLiteralType(x)
              )
              ZonkResult.Done
            }
          case _ =>
            // Not a union type, fill with default literal type
            state.fill(
              tyLhs,
              getLiteralType(x)
            )
            ZonkResult.Done
        }
      } else {
        // No existing value, fill with default literal type
        state.fill(
          tyLhs,
          getLiteralType(x)
        )
        ZonkResult.Done
      }
    }
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
        case (_, Some(Meta(listTLhs))) =>
          state.addPropagator(ListOf(tRhs, listTLhs, cause))
          true
        case (_, Some(l)) if !l.isInstanceOf[ListType] =>
          ck.reporter.apply(TypeMismatch(ListType(AnyType0, meta = None), l, cause))
          true
        case (Some(t1), Some(ListType(t2, _))) =>
          unify(t2, t1, cause)
          true
        case (_, Some(ListType(t2, _))) =>
          unify(t2, tRhs, cause)
          true
        case (Some(t1), None) =>
          unify(this.listTLhs, ListType(t1, meta = None): Term, cause)
          true
        case (None, None) =>
          unify(this.listTLhs, ListType(Meta(tRhs), meta = None): Term, cause)
          true
        case _ => ???
      }
    }

    override def zonk(
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

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean =
      state.readStable(recordTy) match {
        case Some(Meta(id)) =>
          state.addPropagator(RecordFieldPropagator(id, fieldName, expectedTy, cause))
          true
        case Some(recordType) =>
          // Apply type-level reduction to ensure we correctly handle dependent types
          given ctx: Context = summon[Context]
          given ReduceContext = ctx.toReduceContext
          given Reducer = ctx.given_Reducer
          val reducedRecord = DefaultReducer.reduce(recordType, ReduceMode.TypeLevel)

          reducedRecord match {
            case IntegerType(_) | IntType(_) if fieldName == "+" =>
              // Special handling for Integer.+ method
              // For Integer.+, the return type is always Integer
              unify(expectedTy, IntegerType(None), cause)
              true
            case RecordTypeTerm(recordDef, _, _) =>
              recordDef.fields.find(_.name == fieldName) match {
                case Some(fieldTerm) =>
                  // For dependent fields, we may need to further reduce the field type
                  val fieldType = DefaultReducer.reduce(fieldTerm.ty, ReduceMode.TypeLevel)
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

    override def zonk(needed: Vector[CellIdAny])(using state: StateAbility[Tyck], more: Tyck): ZonkResult =
      state.readStable(recordTy) match {
        case None => ZonkResult.Require(Vector(recordTy))
        case _    => ZonkResult.Done
      }
  }

  /** Helper method to check if a source level is compatible with a target level */
  private def isLevelCompatible(source: Term, target: Term)(using
      StateAbility[Tyck],
      Context
  ): Boolean =
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

  // Helper to extract numeric value from a term
  private def extractNumericValue(term: Term): Option[BigInt] = PartialFunction.condOpt(term) {
    case IntTerm(value, _)     => BigInt(value)
    case IntegerTerm(value, _) => value
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
    if (Debug.isEnabled(TraitMatching))
      Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Checking if record ${recordDef.name} implements trait ${traitDef.name}")

    // First check for direct extension relationship
    val hasExtendsClause = recordDef.extendsClause.exists {
      case traitCall: TraitTypeTerm =>
        val matches = traitCall.traitDef.uniqId == traitDef.uniqId
        if (Debug.isEnabled(TraitMatching))
          Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Found extends clause: ${traitCall.traitDef.name}, matches target trait? $matches")
        matches
      case other =>
        if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Found non-trait extends clause: $other")
        false
    }

    if (!hasExtendsClause) {
      if (Debug.isEnabled(TraitMatching))
        Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Record ${recordDef.name} does not extend trait ${traitDef.name}")
      ck.reporter.apply(NotImplementingTrait(recordDef.name, traitDef.name, cause))
      false
    } else {
      // Check that all required fields from the trait are present in the record
      val traitFields = traitDef.body.map(_.statements).getOrElse(Vector.empty).collect {
        case ExprStmtTerm(DefStmtTerm(localv, _, ty, _), _, _) => (localv.name, ty)
        case DefStmtTerm(localv, _, ty, _)                     => (localv.name, ty)
      }

      if (Debug.isEnabled(TraitMatching)) {
        Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Trait ${traitDef.name} fields:")
        traitFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG]   - $name: $ty") }
      }

      val recordFields = recordDef.fields.map(field => (field.name, field.ty)).toMap

      if (Debug.isEnabled(TraitMatching)) {
        Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Record ${recordDef.name} fields:")
        recordFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG]   - $name: $ty") }
      }

      // Check each trait field
      val allFieldsPresent = traitFields.forall { case (fieldName, fieldTy) =>
        recordFields.get(fieldName) match {
          case None =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Missing field $fieldName in record ${recordDef.name}")
            ck.reporter.apply(MissingTraitField(fieldName, recordDef.name, traitDef.name, cause))
            false
          case Some(recordFieldTy) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] Checking type compatibility for field $fieldName")
            // Add type compatibility check
            state.addPropagator(Unify(toId(recordFieldTy), toId(fieldTy), cause))
            true
        }
      }

      if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, t"[TRAIT DEBUG] All fields present and compatible? $allFieldsPresent")
      allFieldsPresent
    }
  }

  // Helper method to check if one trait extends another
  private def checkTraitExtends(
      childTraitDef: TraitStmtTerm,
      parentTraitDef: TraitStmtTerm,
      _cause: Expr
  )(using
      Context,
      Tyck,
      StateAbility[Tyck]
  ): Boolean = {
    // Check if they're the same trait (reflexivity)
    if (childTraitDef.uniqId == parentTraitDef.uniqId) {
      return true
    }

    // Check direct parent
    val directParent = childTraitDef.extendsClause match {
      case Some(traitCall: TraitTypeTerm) =>
        traitCall.traitDef.uniqId == parentTraitDef.uniqId
      case _ => false
    }

    directParent
  }

  // Add helper methods for union subtyping compatibility checking
  private def unionUnionCompatible(types1: NonEmptyVector[Term], types2: NonEmptyVector[Term])(using
      StateAbility[Tyck],
      Context
  ): Boolean =
    // For each type in RHS union, at least one type in LHS union must accept it
    types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))

  private def specificUnionCompatible(specificType: Term, unionTypes: NonEmptyVector[Term])(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    // For a specific type to be compatible with a union type,
    // the specific type must be compatible with at least one of the union components
    // This is for cases like: let y: Integer | String = x
    // where x: Integer
    if (Debug.isEnabled(UnionMatching))
      Debug.debugPrint(
        UnionMatching,
        t"[UNION DEBUG] Checking if $specificType is compatible with at least one of union components: ${unionTypes.mkString(", ")}"
      )
    val result = unionTypes.exists { unionType =>
      val compatible = tryUnify(specificType, unionType)
      if (Debug.isEnabled(UnionMatching))
        Debug.debugPrint(UnionMatching, t"[UNION DEBUG]   Component check: $specificType compatible with $unionType? $compatible")
      compatible
    }
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"[UNION DEBUG] Final result: $result")
    result
  }

  private def unionSpecificCompatible(unionTypes: NonEmptyVector[Term], specificType: Term)(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    // For a union type to be compatible with a specific type,
    // at least one type in the union must be compatible with the specific type
    // This is for cases like: let x: Integer | String; let y: SomeType = x;
    // where y: SomeType must accept at least one of Integer or String
    if (Debug.isEnabled(UnionMatching))
      Debug.debugPrint(UnionMatching, t"[UNION DEBUG] Checking if any union component ${unionTypes.mkString(", ")} is compatible with $specificType")
    val result = unionTypes.exists { unionType =>
      val compatible = tryUnify(unionType, specificType)
      if (Debug.isEnabled(UnionMatching))
        Debug.debugPrint(UnionMatching, t"[UNION DEBUG]   Component check: $unionType compatible with $specificType? $compatible")
      compatible
    }
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, t"[UNION DEBUG] Final result: $result")
    result
  }

  /**
   * Helper method to ensure a cell has a default value
   * This creates a cell with a default value to avoid "not covered by propagator" errors
   */
  def ensureDefaultValue[T](defaultValue: T)(using state: StateAbility[Tyck]): CellId[T] = {
    withDefault(defaultValue)
  }

  /**
   * Propagator to verify that a record properly implements a trait
   */
  case class CheckTraitImplementation(
      recordDef: RecordStmtTerm,
      traitDef: TraitStmtTerm,
      cause: Expr
  )(using Context) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set.empty
    override val writingCells: Set[CIdOf[Cell[?]]] = Set.empty
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set.empty

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      // Delegate to the checkTraitImplementation method
      // We don't need to check the result - any errors will be reported directly
      checkTraitImplementation(recordDef, traitDef, cause)
      // Always return true to ensure the propagator is removed
      true
    }

    override def zonk(needed: Vector[CellIdAny])(using state: StateAbility[Tyck], more: Tyck): ZonkResult = 
      ZonkResult.Done
  }
}
