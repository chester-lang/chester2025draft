package chester.tyck

import chester.error.*
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.syntax.{AbsoluteRef, ModuleRef}
import chester.uniqid.Uniqid

trait TyckPropagator extends ElaboraterCommon {

  /** Unifies two terms, ensuring they have compatible types.
    * This is a core part of type checking that handles:
    * - Meta variable resolution
    * - Structural type unification
    * - Type level compatibility
    */
  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    (lhs, rhs) match {
      // Handle meta variables
      case (Meta(x: CellId[Term]), y) =>
        val yId = toId(y)
        if (x != yId) { // Prevent self-unification
          state.addPropagator(Unify(x, yId, cause))
        }
      case (x, Meta(y: CellId[Term])) =>
        val xId = toId(x)
        if (xId != y) { // Prevent self-unification
          state.addPropagator(Unify(xId, y, cause))
        }

      // Handle intersection types
      case (x, Intersection(xs, _)) =>
        if (xs.exists(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

      // Structural unification for tuple types
      case (TupleType(types1, _), TupleType(types2, _)) if types1.length == types2.length =>
        types1.zip(types2).foreach { case (t1, t2) =>
          unify(t1, t2, cause)
        }

      // Structural unification for list types
      case (ListType(elem1, _), ListType(elem2, _)) =>
        unify(elem1, elem2, cause)

      // Handle type universe levels
      case (Type(level1, _), Type(level2, _)) =>
        unify(level1, level2, cause)

      // Special cases for type levels
      case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => ()
      case (LevelFinite(_, _), LevelUnrestricted(_)) => ()

      // Base case: terms are equal
      case (x, y) if x == y =>
        ()

      // Base case: types do not match
      case _ =>
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))
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
      if (lhs.isDefined && rhs.isDefined) {
        unify(lhs.get, rhs.get, cause)
        return true
      }
      (lhs, rhs) match {
        case (Some(Meta(lhs)), _) => {
          state.addPropagator(Unify(lhs, this.rhs, cause))
          return true
        }
        case (_, Some(Meta(rhs))) => {
          state.addPropagator(Unify(this.lhs, rhs, cause))
          return true
        }
        case _ => ()
      }
      return false
    }

    override def naiveZonk(
        needed: Vector[CellIdAny]
    )(using state: StateAbility[Tyck], more: Tyck): ZonkResult = {
      val lhs = state.readStable(this.lhs)
      val rhs = state.readStable(this.rhs)
      (lhs, rhs) match {
        case (Some(lhs), Some(rhs)) if lhs == rhs => return ZonkResult.Done
        case (Some(lhs), None) => {
          state.fill(this.rhs, lhs)
          return ZonkResult.Done
        }
        case (None, Some(rhs)) => {
          state.fill(this.lhs, rhs)
          return ZonkResult.Done
        }
        case _ => return ZonkResult.Require(Vector(this.lhs, this.rhs))
      }
    }
  }

  /** Attempts to unify two terms without reporting errors.
    * Used for testing type compatibility without side effects.
    * Returns true if unification is possible.
    */
  def tryUnify(lhs: Term, rhs: Term)(using
      state: StateAbility[Tyck],
      localCtx: Context
  ): Boolean = {
    if (lhs == rhs) return true
    val lhsResolved = lhs match {
      case varCall: ReferenceCall =>
        localCtx.getKnown(varCall) match {
          case Some(tyAndVal) =>
            state.readStable(tyAndVal.valueId).getOrElse(lhs)
          case None => lhs
        }
      case _ => lhs
    }
    val rhsResolved = rhs match {
      case varCall: ReferenceCall =>
        localCtx.getKnown(varCall) match {
          case Some(tyAndVal) =>
            state.readStable(tyAndVal.valueId).getOrElse(rhs)
          case None => rhs
        }
      case _ => rhs
    }
    if (lhsResolved == rhsResolved) return true

    (lhsResolved, rhsResolved) match {
      case (Type(level1, _), Type(level2, _)) =>
        level1 == level2

      case (ListType(elem1, _), ListType(elem2, _)) =>
        tryUnify(elem1, elem2)

      case (LevelFinite(_, _), LevelUnrestricted(_)) => true
      case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => true

      case _ => false
    }
  }

  case class LiteralType(x: Literals, tyLhs: CellId[Term])(using
      Context
  ) extends Propagator[Tyck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(tyLhs)

    override def run(using state: StateAbility[Tyck], more: Tyck): Boolean = {
      if (state.noStableValue(tyLhs)) return false
      val ty_ = state.readStable(this.tyLhs).get
      ty_ match {
        case Meta(ty) => {
          state.addPropagator(LiteralType(x, ty))
          return true
        }
        case _ => ()
      }
      x match {
        case IntegerLiteral(value, _) => {
          if (value.isValidInt && tryUnify(ty_, IntType(None))) return true
          if (value > 0 && tryUnify(ty_, NaturalType(None))) return true
          val i = Vector(IntegerType(None)) ++
            Vector(NaturalType(None)).filter(x => value > 0) ++
            Vector(IntType(None)).filter(x => value.isValidInt) ++
            Vector(UIntType(None)).filter(x => value > 0 && value.isValidInt)
          unify(ty_, Intersection(i.assumeNonEmpty, None), x)
          return true
        }
        case RationalLiteral(_, _) => {
          unify(ty_, RationalType(None), x)
          return true
        }
        case StringLiteral(_, _) => {
          unify(ty_, StringType(None), x)

          return true
        }
        case SymbolLiteral(_, _) => {
          unify(ty_, SymbolType(None), x)
          return true
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

  /** Propagator for handling union types.
    * Ensures that each right-hand side type is assignable to the left-hand side type.
    */
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

        // Check that each rhsValue is assignable to lhsValue
        val assignable = rhsValues.forall { rhsValue =>
          // Skip self-unification to prevent infinite loops
          if (lhsValue == rhsValue) true
          else {
            unify(lhsValue, rhsValue, cause)
            true // Assuming unify reports errors internally
          }
        }
        assignable
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

      val unknownRhs = rhs.zip(rhsValuesOpt).collect { case (id, None) => id }
      if (unknownRhs.nonEmpty) {
        // Wait for all rhs values to be known
        ZonkResult.Require(unknownRhs.toVector)
      } else {
        val rhsValues = rhsValuesOpt.map(_.get)

        lhsValueOpt match {
          case Some(lhsValue) =>
            // LHS is known, unify each RHS with LHS
            rhsValues.foreach { rhsValue =>
              // Skip self-unification to prevent infinite loops
              if (lhsValue != rhsValue) {
                unify(lhsValue, rhsValue, cause)
              }
            }
            ZonkResult.Done
          case None =>
            // LHS is unknown, create UnionType from RHS values and set LHS
            val unionType = Union_.from(rhsValues.assumeNonEmpty)
            state.fill(lhs, unionType)
            ZonkResult.Done
        }
      }
    }
  }

  /** t is rhs, listT is lhs 
    * Propagator for list type unification
    */
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
        case Some(RecordCallTerm(recordDef, _, _)) =>
          recordDef.fields.find(_.name == fieldName) match {
            case Some(fieldTerm) =>
              unify(expectedTy, fieldTerm.ty, cause)
              true
            case None =>
              val problem = FieldNotFound(fieldName, recordDef.name, cause)
              more.reporter.apply(problem)
              true
          }
        case Some(other) =>
          val problem = NotARecordType(other, cause)
          more.reporter.apply(problem)
          true
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

}
