package chester.tyck

import chester.error.*
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.reduce.{NaiveReducer, ReduceContext, ReduceMode, Reducer}
import cats.data.NonEmptyVector

import scala.language.implicitConversions
import java.util.concurrent.atomic.AtomicInteger

// Import Debug categories
import chester.utils.Debug.DebugCategory._

trait TyckPropagator extends ElaboraterCommon {

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    def addUnificationPropagator(lhsId: CellId[Term], rhsId: CellId[Term]) = {
      state.addPropagator(Unify(lhsId, rhsId, cause))
    }

    if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying $lhs with $rhs")

    // Handle meta variables
    (toTerm(lhs), toTerm(rhs)) match {
      case (Meta(cellId), rhs) => {
        addUnificationPropagator(cellId, toId(rhs))
      }
      case (lhs, Meta(cellId)) => {
        addUnificationPropagator(toId(lhs), cellId)
      }
      case (lhs, rhs) if lhs != rhs =>
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        val lhsResolved = NaiveReducer.reduce(lhs, ReduceMode.TypeLevel)
        val rhsResolved = NaiveReducer.reduce(rhs, ReduceMode.TypeLevel)
        if (lhsResolved == rhsResolved) return

        (lhsResolved, rhsResolved) match {
          // Handle Union types - rhs must be a subtype of lhs
          case (lhsType, Union(types2, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying lhsType $lhsType with union type ${types2.mkString(", ")}")
            // For a union on the right (like Integer | String),
            // the left side type (like Integer) just needs to match ONE component
            val lhsTypeId = toId(lhsType)

            // Find any compatible union component
            val compatibleComponent = types2.find(t2 => tryUnify(lhsType, t2))

            if (compatibleComponent.isDefined) {
              // Create a propagator connecting lhs to the matching component
              addUnificationPropagator(lhsTypeId, toId(compatibleComponent.get))
            } else {
              // No compatible components
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          case (Union(types1, _), rhsType) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying union type ${types1.mkString(", ")} with rhsType $rhsType")
            // For a union on the left, ANY type in the union must be compatible with rhs
            // We need to check if at least one component is compatible
            val anyCompatible = types1.exists(t1 => tryUnify(t1, rhsType))

            if (anyCompatible) {
              // At least one component is compatible, create propagators for that component
              types1.find(t1 => tryUnify(t1, rhsType)).foreach(t1 => addUnificationPropagator(toId(t1), toId(rhsType)))
            } else {
              // No compatible components
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // Record implementing trait (structural subtyping)
          case (RecordTypeTerm(recordDef, _, _), TraitTypeTerm(traitDef, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying record type ${recordDef.name} with trait type ${traitDef.name}")
            if (!checkTraitImplementation(recordDef, traitDef, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} does not implement trait ${traitDef.name}")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            true

          // Record type implementing trait type (structural subtyping)
          case (lhsType @ RecordStmtTerm(name, _, fields, _, extendsClause, _), rhsType @ TraitStmtTerm(traitName, _, _, _, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying record type $name with trait type $traitName")
            if (!checkTraitImplementation(lhsType, rhsType, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record $name does not implement trait $traitName")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            true

          // Record type implementing trait type (structural subtyping)
          case (lhsType @ RecordTypeTerm(recordDef, _, _), rhsType @ TraitTypeTerm(traitDef, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying record type ${recordDef.name} with trait type ${traitDef.name}")
            if (!checkTraitImplementation(recordDef, traitDef, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} does not implement trait ${traitDef.name}")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            true

          // Record type implementing trait type (structural subtyping)
          case (lhsType @ RecordStmtTerm(name, _, fields, _, extendsClause, _), rhsType @ TraitStmtTerm(traitName, _, _, _, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying record type $name with trait type $traitName")
            if (!checkTraitImplementation(lhsType, rhsType, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record $name does not implement trait $traitName")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            true

          // Trait extending trait (structural subtyping)
          case (TraitTypeTerm(childTraitDef, _), TraitTypeTerm(parentTraitDef, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying child trait ${childTraitDef.name} with parent trait ${parentTraitDef.name}")
            if (!checkTraitExtends(childTraitDef, parentTraitDef, cause)) {
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Trait ${childTraitDef.name} does not extend trait ${parentTraitDef.name}")
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }
            true

          // Handle Intersection types
          case (Intersection(types1, _), rhsType) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying intersection type ${types1.mkString(", ")} with rhsType $rhsType")
            if (!types1.exists(t1 => tryUnify(t1, rhsType))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          case (lhsType, Intersection(types2, _)) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying lhsType $lhsType with intersection type ${types2.mkString(", ")}")
            if (!types2.forall(t2 => tryUnify(lhsType, t2))) {
              ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
            }

          // For other cases, add a direct unification propagator
          case (lhsType, rhsType) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Unifying lhsType $lhsType with rhsType $rhsType")
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
          handleUnionUnionPropagation(types1, types2, cause)
          true
        case (Some(lhsType), Some(Union(types2, _))) =>
          handleSpecificUnionPropagation(lhsType, types2, cause)
          true
        case (Some(Union(types1, _)), Some(rhsType)) =>
          handleUnionSpecificPropagation(types1, rhsType, cause)
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
          // First check compatibility
          if (unionUnionCompatible(types1, types2)) {
            // Ensure all types are covered by propagators
            types1.foreach(t => ensureCellIsCovered(toId(t)))
            types2.foreach(t => ensureCellIsCovered(toId(t)))
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(lhsType), Some(Union(types2, _))) =>
          // First check compatibility
          if (specificUnionCompatible(lhsType, types2)) {
            // Ensure all types are covered by propagators
            ensureCellIsCovered(toId(lhsType))
            types2.foreach(t => ensureCellIsCovered(toId(t)))

            // Add a propagator to connect the specific type to the union
            // This is critical for cases like `let y: Integer | String = x` where x: Integer
            // Find each compatible union component and connect the specific type to it
            types2.withFilter(unionType => tryUnify(lhsType, unionType)(using state, summon[Context])).foreach { compatibleType =>
              state.addPropagator(Unify(toId(lhsType), toId(compatibleType), cause)(using summon[Context]))
            }

            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case (Some(Union(types1, _)), Some(rhsType)) =>
          // First check compatibility
          if (unionSpecificCompatible(types1, rhsType)) {
            // Ensure all types are covered by propagators
            types1.foreach(t => ensureCellIsCovered(toId(t)))
            ensureCellIsCovered(toId(rhsType))
            // Connect the compatible union component to the specific type
            connectUnionAndSpecific(this.lhs, types1, this.rhs, rhsType, cause)(using state, more, summon[Context])
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
          if (types1.forall(t1 => tryUnify(t1, rhsType))) {
            ZonkResult.Done
          } else {
            ZonkResult.NotYet
          }
        case _ => ZonkResult.Require(Vector(this.lhs, this.rhs))
      }
    }

    private def handleUnionUnionPropagation(
        types1: NonEmptyVector[Term],
        types2: NonEmptyVector[Term],
        cause: Expr
    )(using
        state: StateAbility[Tyck],
        more: Tyck
    ): Unit = {
      // For each type in rhs union, at least one type in lhs union must accept it
      types2.foreach { t2 =>
        types1.find(t1 => tryUnify(t1, t2)).foreach { t1 =>
          state.addPropagator(Unify(toId(t1), toId(t2), cause))
        }
      }
    }

    private def handleSpecificUnionPropagation(
        specificType: Term,
        unionTypes: NonEmptyVector[Term],
        cause: Expr
    )(using
        state: StateAbility[Tyck],
        more: Tyck
    ): Unit = {
      // For a specific type to be compatible with a union type,
      // the specific type must be compatible with at least one of the union components
      // This is for cases like: let y: Integer | String = x
      // where x: Integer
      if (Debug.isEnabled(UnionMatching))
        Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Checking if $specificType is compatible with at least one of union components: ${unionTypes.mkString(", ")}")

      // Find compatible union components and connect them
      unionTypes.withFilter(unionType => tryUnify(specificType, unionType)).foreach { compatibleType =>
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Found compatible component: $compatibleType")
        state.addPropagator(Unify(toId(specificType), toId(compatibleType), cause))
      }
    }

    private def handleUnionSpecificPropagation(
        unionTypes: NonEmptyVector[Term],
        specificType: Term,
        cause: Expr
    )(using
        StateAbility[Tyck],
        Tyck
    ): Unit = {
      // For a union type to be compatible with a specific type,
      // at least one type in the union must be compatible with the specific type
      // This is for cases like: let x: Integer | String; let y: SomeType = x;
      // where y: SomeType must accept at least one of Integer or String
      if (Debug.isEnabled(UnionMatching))
        Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Checking if any union component ${unionTypes.mkString(", ")} is compatible with $specificType")
      val result = unionTypes.exists(unionType => {
        val compatible = tryUnify(unionType, specificType)
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Component check: $unionType compatible with $specificType? $compatible")
        compatible
      })
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Final result: $result")
    }

    // Helper method to ensure a cell is covered by at least one propagator during zonking
    private def ensureCellIsCovered(cell: CellId[Term])(using
        state: StateAbility[Tyck],
        more: Tyck
    ): Unit = {
      // Create a self-unify propagator for coverage to ensure the cell is covered
      // We can't easily check if it's already covered, so we add a self-unify propagator
      // which is a no-op if the cell is already covered
      state.addPropagator(Unify(cell, cell, EmptyExpr))
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

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      // First check if any of our cells are in the needed list
      val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
      if (ourNeededCells.isEmpty) {
        return ZonkResult.Done // None of our cells are needed
      }

      // Check if we're waiting for rhs values
      val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
      if (unknownRhs.nonEmpty) {
        return ZonkResult.Require(unknownRhs.toVector)
      }

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

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhsValueOpt = state.readStable(lhs)
      val rhsValuesOpt = rhs.map(state.readStable)

      // First check if any of our cells are in the needed list
      val ourNeededCells = (Vector(lhs) ++ rhs).filter(needed.contains)
      if (ourNeededCells.isEmpty) {
        return ZonkResult.Done // None of our cells are needed
      }

      // Check if we're waiting for rhs values
      val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
      if (unknownRhs.nonEmpty) {
        return ZonkResult.Require(unknownRhs.toVector)
      }

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
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG] Trying to unify: $lhs with $rhs")

      // Helper function to fully resolve references in terms
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
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Terms are equal, returning true")
        true
      } else {
        // Use TypeLevel reduction for type equality checking
        given ReduceContext = localCtx.toReduceContext
        given Reducer = localCtx.given_Reducer

        // Fully resolve references for deeper resolution
        val lhsResolved = fullyResolveReference(NaiveReducer.reduce(lhs, ReduceMode.TypeLevel))
        val rhsResolved = fullyResolveReference(NaiveReducer.reduce(rhs, ReduceMode.TypeLevel))

        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   After reduction and resolution: $lhsResolved with $rhsResolved")

        // Special cases for integer literals
        (lhsResolved, rhsResolved) match {
          // Direct matching for integer values (like 42) with Union types containing Integer
          case (v: AbstractIntTerm, Union(types, _)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if union contains Integer for AbstractIntTerm ${v}")
            val hasIntegerType = types.exists(t =>
              t match {
                case IntegerType(_) => true
                case _ =>
                  val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                  reduced == IntegerType(None)
              }
            )
            if (hasIntegerType) {
              if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Union contains Integer, AbstractIntTerm is compatible with union")
              return true
            }

          // The reverse - Union containing Integer is compatible with AbstractIntTerm
          case (Union(types, _), v: AbstractIntTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if union contains Integer for AbstractIntTerm ${v}")
            val hasIntegerType = types.exists(t =>
              t match {
                case IntegerType(_) => true
                case _ =>
                  val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                  reduced == IntegerType(None)
              }
            )
            if (hasIntegerType) {
              if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Union contains Integer, union is compatible with AbstractIntTerm")
              return true
            }

          // Handle the case where a numeric literal needs to match with an Integer type
          case (lit: IntTerm, IntegerType(_)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Integer literal matches IntegerType: true")
            return true
          case (lit: IntegerTerm, IntegerType(_)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Integer literal matches IntegerType: true")
            return true
          // The inverse
          case (IntegerType(_), lit: IntTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   IntegerType matches integer literal: true")
            return true
          case (IntegerType(_), lit: IntegerTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   IntegerType matches integer literal: true")
            return true

          // Special case for literal values like 42 that need to match with Integer type
          case (v: AbstractIntTerm, IntegerType(_)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Abstract integer literal matches IntegerType: true")
            return true
          case (IntegerType(_), v: AbstractIntTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   IntegerType matches abstract integer literal: true")
            return true

          // Special cases for int literals with union types
          case (v: AbstractIntTerm, Union(types, _)) =>
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if AbstractIntTerm $v can be unified with Union ${types.mkString(", ")}")
            // Check if any of the union types is compatible with Integer
            val containsInteger = types.exists {
              case IntegerType(_) =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Union directly contains IntegerType - match found")
                true
              case t =>
                val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                val isInteger = reduced match {
                  case IntegerType(_) => true
                  case _              => false
                }
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Checking reduced type $reduced: isInteger = $isInteger")
                isInteger
            }
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   AbstractIntTerm with Union: contains Integer? $containsInteger")
            if (containsInteger) return true

          case (Union(types, _), v: AbstractIntTerm) =>
            if (Debug.isEnabled(UnionMatching))
              Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if Union ${types.mkString(", ")} can be unified with AbstractIntTerm $v")
            // Check if any of the union types is compatible with Integer
            val containsInteger = types.exists {
              case IntegerType(_) =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Union directly contains IntegerType - match found")
                true
              case t =>
                val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                val isInteger = reduced match {
                  case IntegerType(_) => true
                  case _              => false
                }
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Checking reduced type $reduced: isInteger = $isInteger")
                isInteger
            }
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Union with AbstractIntTerm: contains Integer? $containsInteger")
            if (containsInteger) return true

          // Handle IntTerm cases with Union (specific for 42 and similar literals)
          case (v: IntTerm, Union(types, _)) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if IntTerm $v can be unified with Union ${types.mkString(", ")}")
            // Check if any of the union types is compatible with Integer
            val containsInteger = types.exists {
              case IntegerType(_) =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Union directly contains IntegerType - match found")
                true
              case t =>
                val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                val isInteger = reduced match {
                  case IntegerType(_) => true
                  case _              => false
                }
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Checking reduced type $reduced: isInteger = $isInteger")
                isInteger
            }
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   IntTerm with Union: contains Integer? $containsInteger")
            if (containsInteger) return true

          case (Union(types, _), v: IntTerm) =>
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking if Union ${types.mkString(", ")} can be unified with IntTerm $v")
            // Check if any of the union types is compatible with Integer
            val containsInteger = types.exists {
              case IntegerType(_) =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Union directly contains IntegerType - match found")
                true
              case t =>
                val reduced = NaiveReducer.reduce(t, ReduceMode.TypeLevel)
                val isInteger = reduced match {
                  case IntegerType(_) => true
                  case _              => false
                }
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]     Checking reduced type $reduced: isInteger = $isInteger")
                isInteger
            }
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Union with IntTerm: contains Integer? $containsInteger")
            if (containsInteger) return true

          case _ => // Continue with normal processing
        }

        if (lhsResolved == rhsResolved) {
          if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Reduced terms are equal, returning true")
          true
        } else {
          // If structural equality check fails, try alpha-equivalence
          // which is crucial for dependent type systems
          if (areAlphaEquivalent(lhsResolved, rhsResolved)) {
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Terms are alpha-equivalent, returning true")
            true
          } else {
            (lhsResolved, rhsResolved) match {
              case (Type(level1, _), Type(level2, _)) =>
                val result = isLevelCompatible(level1, level2)(using state, localCtx)
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Type level check: $level1 compatible with $level2? $result")
                result

              case (ListType(elem1, _), ListType(elem2, _)) =>
                val result = tryUnifyInternal(elem1, elem2, depth + 1)
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   List element check: $result")
                result

              case (lhsType, Union(types2, _)) =>
                // For a specific type and a union type, check if the specific type
                // is compatible with at least one union component
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking specific type against union: $lhsType with ${types2.mkString(", ")}")
                val result = types2.exists(t2 => tryUnifyInternal(lhsType, t2, depth + 1))
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Specific-to-union check: $result")
                result

              case (Union(types1, _), rhsType) =>
                // For a union type and a specific type, check if at least one union component
                // is compatible with the specific type (NOT all components need to be compatible)
                if (Debug.isEnabled(UnionMatching))
                  Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Checking union type against specific: ${types1.mkString(", ")} with $rhsType")
                val result = types1.exists(t1 => tryUnifyInternal(t1, rhsType, depth + 1))
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   Union-to-specific check: $result")
                result

              case _ =>
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"${indent}[UNIFY DEBUG]   No match found, returning false")
                false
            }
          }
        }
      }
    }

    // Start the recursion
    val result = tryUnifyInternal(lhs, rhs, 0)
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNIFY DEBUG] Final result for unifying $lhs with $rhs: $result")
    result
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
                if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL DEBUG] Processing integer literal $value with expected type $ty_")
                // Try to handle union types
                ty_ match {
                  case Union(types, _) =>
                    // Check if any of the union types is compatible with this integer literal
                    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL DEBUG] Checking integer literal $value with union type $ty_")
                    val compatibleTypes = types.filter(unionType => {
                      // Check for Integer type or its reduced form
                      unionType match {
                        case IntegerType(_) => true
                        case _ =>
                          val reduced =
                            NaiveReducer.reduce(unionType, ReduceMode.TypeLevel)(using summon[Context].toReduceContext, summon[Context].given_Reducer)
                          reduced match {
                            case IntegerType(_) => true
                            case _              => false
                          }
                      }
                    })

                    if (compatibleTypes.nonEmpty) {
                      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL DEBUG] Found compatible types in union: ${compatibleTypes.mkString(", ")}")
                      // Found at least one compatible type in the union
                      true
                    } else {
                      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL DEBUG] No compatible types found in union for integer literal $value")
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
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // First, check if we already have a value for this cell
      val existingValue = state.readStable(tyLhs)
      if (existingValue.isDefined) {
        existingValue.get match {
          case Union(types, _) =>
            // If we have a union type, we need to make sure the literal is compatible with at least one component
            if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL ZONK DEBUG] Handling union type during zonk: $existingValue")
            val literalType = x match {
              case IntegerLiteral(_, _)  => IntegerType(None)
              case RationalLiteral(_, _) => RationalType(None)
              case StringLiteral(_, _)   => StringType(None)
              case SymbolLiteral(_, _)   => SymbolType(None)
            }

            // Find compatible types
            val compatibleTypes = types.filter(unionType => tryUnify(literalType, unionType)(using state, summon[Context]))

            if (compatibleTypes.nonEmpty) {
              if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL ZONK DEBUG] Found compatible union components: ${compatibleTypes.mkString(", ")}")
              ZonkResult.Done // Leave the union type as is
            } else {
              if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[LITERAL ZONK DEBUG] No compatible union components found for literal type $literalType")
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
          case _ =>
            // Not a union type, fill with default literal type
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
      } else {
        // No existing value, fill with default literal type
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
            case IntegerType(_) | IntType(_) if fieldName == "+" =>
              // Special handling for Integer.+ method
              // For Integer.+, the return type is always Integer
              unify(expectedTy, IntegerType(None), cause)
              true
            case RecordTypeTerm(recordDef, _, _) =>
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
    if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Checking if record ${recordDef.name} implements trait ${traitDef.name}")

    // First check for direct extension relationship
    val hasExtendsClause = recordDef.extendsClause.exists {
      case traitCall: TraitTypeTerm =>
        val matches = traitCall.traitDef.uniqId == traitDef.uniqId
        if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Found extends clause: ${traitCall.traitDef.name}, matches target trait? $matches")
        matches
      case other =>
        if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Found non-trait extends clause: $other")
        false
    }

    if (!hasExtendsClause) {
      if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} does not extend trait ${traitDef.name}")
      ck.reporter.apply(NotImplementingTrait(recordDef.name, traitDef.name, cause))
      false
    } else {
      // Check that all required fields from the trait are present in the record
      val traitFields = traitDef.body.map(_.statements).getOrElse(Vector.empty).collect {
        case ExprStmtTerm(DefStmtTerm(localv, _, ty, _), _, _) => (localv.name, ty)
        case DefStmtTerm(localv, _, ty, _)                     => (localv.name, ty)
      }

      if (Debug.isEnabled(TraitMatching)) {
        Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Trait ${traitDef.name} fields:")
        traitFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG]   - $name: $ty") }
      }

      val recordFields = recordDef.fields.map(field => (field.name, field.ty)).toMap

      if (Debug.isEnabled(TraitMatching)) {
        Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} fields:")
        recordFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG]   - $name: $ty") }
      }

      // Check each trait field
      val allFieldsPresent = traitFields.forall { case (fieldName, fieldTy) =>
        recordFields.get(fieldName) match {
          case None =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Missing field $fieldName in record ${recordDef.name}")
            ck.reporter.apply(MissingTraitField(fieldName, recordDef.name, traitDef.name, cause))
            false
          case Some(recordFieldTy) =>
            if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Checking type compatibility for field $fieldName")
            // Add type compatibility check
            state.addPropagator(Unify(toId(recordFieldTy), toId(fieldTy), cause))
            true
        }
      }

      if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] All fields present and compatible? $allFieldsPresent")
      allFieldsPresent
    }
  }

  // Helper method to check if one trait extends another
  private def checkTraitExtends(
      childTraitDef: TraitStmtTerm,
      parentTraitDef: TraitStmtTerm,
      cause: Expr
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
  ): Boolean = {
    // For each type in RHS union, at least one type in LHS union must accept it
    types2.forall(t2 => types1.exists(t1 => tryUnify(t1, t2)))
  }

  private def specificUnionCompatible(specificType: Term, unionTypes: NonEmptyVector[Term])(using
      StateAbility[Tyck],
      Context
  ): Boolean = {
    // For a specific type to be compatible with a union type,
    // the specific type must be compatible with at least one of the union components
    // This is for cases like: let y: Integer | String = x
    // where x: Integer
    if (Debug.isEnabled(UnionMatching))
      Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Checking if $specificType is compatible with at least one of union components: ${unionTypes.mkString(", ")}")
    val result = unionTypes.exists(unionType => {
      val compatible = tryUnify(specificType, unionType)
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Component check: $specificType compatible with $unionType? $compatible")
      compatible
    })
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Final result: $result")
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
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Checking if any union component ${unionTypes.mkString(", ")} is compatible with $specificType")
    val result = unionTypes.exists(unionType => {
      val compatible = tryUnify(unionType, specificType)
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Component check: $unionType compatible with $specificType? $compatible")
      compatible
    })
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Final result: $result")
    result
  }

  // Propagator to ensure a cell is covered
  case class EnsureCellCoverage(
      cell: CellId[Term],
      cause: Expr
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(cell.asInstanceOf[CIdOf[Cell[?]]])
    override val writingCells: Set[CIdOf[Cell[?]]] = Set.empty
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(cell.asInstanceOf[CIdOf[Cell[?]]])

    override def run(using StateAbility[Tyck], Tyck): Boolean = {
      // This propagator simply ensures the cell has at least one propagator
      // It always succeeds immediately
      true
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using StateAbility[Tyck], Tyck): ZonkResult = {
      // Nothing to zonk - just ensure the cell is included
      ZonkResult.Done
    }
  }

  // Connect a union type to a specific type for compatibility checks
  private def connectUnionAndSpecific(
      unionId: CellId[Term],
      unionTypes: NonEmptyVector[Term],
      specificId: CellId[Term],
      specificType: Term,
      cause: Expr
  )(using
      state: StateAbility[Tyck],
      more: Tyck,
      ctx: Context
  ): Unit = {
    // For a union type and a specific type, find the compatible component
    // and connect it to the specific type
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Connecting union ${unionTypes.mkString(", ")} with specific $specificType")
    unionTypes.find(unionType => tryUnify(unionType, specificType)(using state, ctx)).foreach { compatibleType =>
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Found compatible component: $compatibleType")
      state.addPropagator(Unify(toId(compatibleType), specificId, cause)(using ctx))
    }
  }

  // Connect a specific type to a union type for compatibility checks
  def connectSpecificAndUnion(
      specificId: CellId[Term],
      specificType: Term,
      unionId: CellId[Term],
      unionTypes: NonEmptyVector[Term],
      cause: Expr
  )(using
      state: StateAbility[Tyck],
      more: Tyck,
      ctx: Context
  ): Unit = {
    // For a specific type and a union type, we need to:
    // 1. Find all compatible union components
    // 2. Connect the specific type to each compatible component
    // 3. Connect the specific type directly to the union type
    // 4. Connect the union to all of its components (crucial)
    // 5. Make sure all cells have coverage

    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Connecting specific $specificType with union ${unionTypes.mkString(", ")}")
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Specific Type ID: $specificId")
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Union Type ID: $unionId")
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Union Types: ${unionTypes.map(t => s"$t (${t.getClass.getSimpleName})").mkString(", ")}")
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Cause: $cause")

    // Resolve and reduce types to handle references properly
    given ReduceContext = ctx.toReduceContext
    given Reducer = ctx.given_Reducer

    val reducedSpecificType = NaiveReducer.reduce(specificType, ReduceMode.TypeLevel)
    val reducedUnionTypes = unionTypes.map(unionType => NaiveReducer.reduce(unionType, ReduceMode.TypeLevel))

    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Reduced specific type: $reducedSpecificType")
    if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Reduced union types: ${reducedUnionTypes.mkString(", ")}")

    // Normal processing for other types
    val compatibleComponents = reducedUnionTypes.filter(unionType => {
      val isCompatible = tryUnify(reducedSpecificType, unionType)(using state, ctx)
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Checking compatibility: $reducedSpecificType with $unionType - Result: $isCompatible")
      isCompatible
    })
    (compatibleComponents, compatibleComponents.nonEmpty)

    if (compatibleComponents.nonEmpty) {
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG] Found compatible components: ${compatibleComponents.mkString(", ")}")

      // Get a vector of all component cell IDs first
      val unionComponentCellIds = unionTypes.map(t => toId(t).asInstanceOf[CellId[Term]]).toVector

      // Step 1: Connect the specific type to each compatible component
      compatibleComponents.foreach { compatibleType =>
        val componentId = toId(compatibleType)
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Connecting specific to component: $compatibleType (Cell ID: $componentId)")
        state.addPropagator(Unify(specificId, componentId, cause)(using ctx))
      }

      // Step 2: Connect the specific type directly to the union
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Adding direct connection from specific $specificId to union $unionId")
      state.addPropagator(Unify(specificId, unionId, cause)(using ctx))

      // Step 3: Connect the union to all of its components using UnionOf
      if (Debug.isEnabled(UnionMatching))
        Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Creating UnionOf propagator: union $unionId to components: ${unionComponentCellIds.mkString(", ")}")
      state.addPropagator(UnionOf(unionId, unionComponentCellIds, cause))

      // Step 4: Ensure cell coverage for all cells
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Adding coverage for specific: $specificId")
      state.addPropagator(EnsureCellCoverage(specificId, cause))
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Adding coverage for union: $unionId")
      state.addPropagator(EnsureCellCoverage(unionId, cause))
      unionComponentCellIds.foreach { componentId =>
        if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   Adding coverage for component: $componentId")
        state.addPropagator(EnsureCellCoverage(componentId, cause))
      }
    } else {
      // If no compatible components found, report an error
      if (Debug.isEnabled(UnionMatching)) Debug.debugPrint(UnionMatching, s"[UNION DEBUG]   No compatible components found between $specificType and union")
      more.reporter.apply(TypeMismatch(specificType, Union(unionTypes, None), cause))
    }
  }

  case class CheckTraitImplementation(
      recordDef: RecordStmtTerm,
      traitDef: TraitStmtTerm,
      cause: Expr
  )(using Context)
      extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set.empty
    override val writingCells: Set[CIdOf[Cell[?]]] = Set.empty
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set.empty

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Checking if record ${recordDef.name} implements trait ${traitDef.name}")

      // First check for direct extension relationship
      val hasExtendsClause = recordDef.extendsClause.exists {
        case traitCall: TraitTypeTerm =>
          val matches = traitCall.traitDef.uniqId == traitDef.uniqId
          if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Found extends clause: ${traitCall.traitDef.name}, matches target trait? $matches")
          matches
        case other =>
          if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Found non-trait extends clause: $other")
          false
      }

      if (!hasExtendsClause) {
        if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} does not extend trait ${traitDef.name}")
        more.reporter.apply(NotImplementingTrait(recordDef.name, traitDef.name, cause))
        false
      } else {
        // Check that all required fields from the trait are present in the record
        val traitFields = traitDef.body.map(_.statements).getOrElse(Vector.empty).collect {
          case ExprStmtTerm(DefStmtTerm(localv, _, ty, _), _, _) => (localv.name, ty)
          case DefStmtTerm(localv, _, ty, _)                     => (localv.name, ty)
        }

        if (Debug.isEnabled(TraitMatching)) {
          Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Trait ${traitDef.name} fields:")
          traitFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG]   - $name: $ty") }
        }

        val recordFields = recordDef.fields.map(field => (field.name, field.ty)).toMap

        if (Debug.isEnabled(TraitMatching)) {
          Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Record ${recordDef.name} fields:")
          recordFields.foreach { case (name, ty) => Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG]   - $name: $ty") }
        }

        // Check each trait field
        val allFieldsPresent = traitFields.forall { case (fieldName, fieldTy) =>
          recordFields.get(fieldName) match {
            case None =>
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Missing field $fieldName in record ${recordDef.name}")
              more.reporter.apply(MissingTraitField(fieldName, recordDef.name, traitDef.name, cause))
              false
            case Some(recordFieldTy) =>
              if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] Checking type compatibility for field $fieldName")
              // Add type compatibility check
              state.addPropagator(Unify(toId(recordFieldTy), toId(fieldTy), cause))
              true
          }
        }

        if (Debug.isEnabled(TraitMatching)) Debug.debugPrint(TraitMatching, s"[TRAIT DEBUG] All fields present and compatible? $allFieldsPresent")
        allFieldsPresent
      }
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      // Nothing to zonk - just ensure the propagator has run
      ZonkResult.Done
    }
  }

}
