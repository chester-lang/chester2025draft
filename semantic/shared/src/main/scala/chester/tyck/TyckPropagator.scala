package chester.tyck

import cats.implicits.*
import chester.error.{Problem, Reporter, TyckProblem, VectorReporter, WithServerity}
import chester.syntax.{Name, LoadedModules, ModuleRef, TAST, DefaultModule}
import chester.syntax.concrete.{Expr, ExprMeta}
import chester.syntax.core.{Term, Effects, Meta, Typeω}
import chester.syntax.core.spec.{given, given_TypeF_Term_Type}
import chester.tyck.api.{SemanticCollector, SymbolCollector}
import chester.utils.{MutBox, flatMapOrdered, hasDuplication, assumeNonEmpty}
import chester.utils.propagator.{StateAbility, Propagator, ZonkResult, ProvideCellId, Cell}
import chester.reduce.{Reducer, NaiveReducer, ReduceContext, ReduceMode}
import chester.reduce.ReduceContext.given_Conversion_Context_ReduceContext
import chester.uniqid.{Uniqid, UniqidOf}
import chester.resolve.{SimpleDesalt, resolveOpSeq}

import scala.language.implicitConversions

trait TyckPropagator extends ProvideCtx with ElaboraterBase {

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
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
      case (Meta(lhs), rhs) => 
        // Explicitly specify which overload to use
        unify(lhs: CellId[Term], rhs: Term, cause)
      case (lhs, Meta(rhs)) => 
        // Explicitly specify which overload to use
        unify(lhs: Term, rhs: CellId[Term], cause)

      // Structural unification for ListType
      case (ListType(elem1, _), ListType(elem2, _)) =>
        unify(elem1, elem2, cause)

      case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => ()

      case (x, Intersection(xs, _)) =>
        if (xs.exists(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause))

      // Structural unification for TupleType
      case (TupleType(types1, _), TupleType(types2, _)) if types1.length == types2.length =>
        types1.zip(types2).foreach { case (t1, t2) => unify(t1, t2, cause) }

      // Type levels: unify levels
      case (Type(level1, _), Type(level2, _)) =>
        unify(level1, level2, cause)

      case (LevelFinite(_, _), LevelUnrestricted(_)) => ()

      case (Union(_, _), Union(_, _)) => ???

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
          unify(lhsValue, rhsValue, cause)
          true // Assuming unify reports errors internally
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
              unify(lhsValue, rhsValue, cause)
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

  def tryUnify(lhs: Term, rhs: Term)(using
      state: StateAbility[Tyck],
      localCtx: Context
  ): Boolean = {
    if (lhs == rhs) return true
    // Use TypeLevel reduction for type equality checking
    given ReduceContext = localCtx.toReduceContext
    given Reducer = localCtx.given_Reducer
    val lhsResolved = NaiveReducer.reduce(lhs, ReduceMode.TypeLevel) match {
      case varCall: ReferenceCall =>
        localCtx.getKnown(varCall) match {
          case Some(tyAndVal) =>
            state.readStable(tyAndVal.valueId).getOrElse(lhs)
          case None => lhs
        }
      case other => other
    }
    val rhsResolved = NaiveReducer.reduce(rhs, ReduceMode.TypeLevel) match {
      case varCall: ReferenceCall =>
        localCtx.getKnown(varCall) match {
          case Some(tyAndVal) =>
            state.readStable(tyAndVal.valueId).getOrElse(rhs)
          case None => rhs
        }
      case other => other
    }
    if (lhsResolved == rhsResolved) return true

    (lhsResolved, rhsResolved) match {
      case (Type(level1, _), Type(level2, _)) =>
        // Try to reduce both types in type-level mode
        val reducedLhs = NaiveReducer.reduce(lhsResolved, ReduceMode.TypeLevel)
        val reducedRhs = NaiveReducer.reduce(rhsResolved, ReduceMode.TypeLevel)
        if (reducedLhs != lhsResolved || reducedRhs != rhsResolved) {
          tryUnify(reducedLhs, reducedRhs)
        } else {
          level1 == level2
        }

      case (ListType(elem1, _), ListType(elem2, _)) =>
        tryUnify(elem1, elem2)

      case (Union(_, _), Union(_, _)) => ???

      case (Intersection(_, _), Intersection(_, _)) => ???

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
        case Some(term) =>
          // Try type-level reduction first
          given ReduceContext = summon[Context].toReduceContext
          given Reducer = summon[Context].given_Reducer
          val reducedTerm = NaiveReducer.reduce(term, ReduceMode.TypeLevel)
          if (reducedTerm != term) {
            // If type-level reduction worked, try again with reduced term
            state.addPropagator(RecordFieldPropagator(literal(reducedTerm), fieldName, expectedTy, cause))
            true
          } else {
            // If type-level reduction didn't work, try normal reduction
            val normalReducedTerm = NaiveReducer.reduce(term, ReduceMode.Normal)
            if (normalReducedTerm != term) {
              state.addPropagator(RecordFieldPropagator(literal(normalReducedTerm), fieldName, expectedTy, cause))
              true
            } else {
              // If both reductions fail, try one more time with type-level mode on the term's type
              term match {
                case Type(level, _) =>
                  // If we get a Type term, try to reduce it to a record type
                  val reducedType = NaiveReducer.reduce(term, ReduceMode.TypeLevel)
                  if (reducedType != term) {
                    state.addPropagator(RecordFieldPropagator(literal(reducedType), fieldName, expectedTy, cause))
                    true
                  } else {
                    more.reporter.apply(NotARecordType(term, cause))
                    true
                  }
                case FCallTerm(f, args, _) =>
                  // Try to reduce the function and args aggressively
                  val reducedF = NaiveReducer.reduce(f, ReduceMode.TypeLevel)
                  val reducedArgs = args.map(calling =>
                    Calling(
                      calling.args.map(arg => CallingArgTerm(
                        NaiveReducer.reduce(arg.value, ReduceMode.TypeLevel),
                        NaiveReducer.reduce(arg.ty, ReduceMode.TypeLevel),
                        arg.name,
                        arg.vararg,
                        arg.meta
                      )),
                      calling.implicitly,
                      calling.meta
                    )
                  )
                  
                  // First try to evaluate the function
                  reducedF match {
                    case Function(FunctionType(telescopes, retTy, _, _), body, _) =>
                      // Substitute args into body
                      val substitutedBody = telescopes.zip(reducedArgs).foldLeft(body) { case (acc, (telescope, calling)) =>
                        telescope.args.zip(calling.args).foldLeft(acc) { case (acc, (param, arg)) =>
                          acc.substitute(param.bind, arg.value)
                        }
                      }
                      // Try reducing the substituted body
                      val evaluated = NaiveReducer.reduce(substitutedBody, ReduceMode.TypeLevel)
                      if (evaluated != substitutedBody) {
                        state.addPropagator(RecordFieldPropagator(literal(evaluated), fieldName, expectedTy, cause))
                        true
                      } else {
                        more.reporter.apply(NotARecordType(term, cause))
                        true
                      }
                    case _ =>
                      more.reporter.apply(NotARecordType(term, cause))
                      true
                  }
                case ref: ReferenceCall =>
                  // Try to resolve and reduce the reference
                  val resolved = summon[Context].toReduceContext.resolve(ref)
                  if (resolved != ref) {
                    state.addPropagator(RecordFieldPropagator(literal(resolved), fieldName, expectedTy, cause))
                    true
                  } else {
                    more.reporter.apply(NotARecordType(term, cause))
                    true
                  }
                case RecordCallTerm(recordDef, _, _) =>
                  recordDef.fields.find(_.name == fieldName) match {
                    case Some(fieldTerm) =>
                      unify(expectedTy, fieldTerm.ty, cause)
                      true
                    case None =>
                      more.reporter.apply(FieldNotFound(fieldName.toString, recordDef.name, cause))
                      true
                  }
                case _ =>
                  more.reporter.apply(NotARecordType(term, cause))
                  true
              }
            }
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

  case class RecordField(recordTy: MetaTerm, field: String, fieldTy: MetaTerm) extends TyckPropagator {
    override def propagate(using ctx: ElaborateContext): Boolean = {
      // Try to resolve the record type meta term
      ctx.state.resolve(recordTy) match {
        case Some(resolvedTy: Term) =>
          // Record type resolved, try to get field type
          val reducedTy = summon[Reducer].reduce(resolvedTy, ReduceMode.TypeLevel)
          reducedTy match {
            case recordType: RecordStmtTerm =>
              // Found record type, look up field
              recordType.fields.find(_.name == field) match {
                case Some(fieldTerm) =>
                  // Field found, unify with field type meta term
                  ctx.state.addPropagator(Unify(fieldTy, fieldTerm.ty))
                  true
                case None =>
                  // Field not found, report error
                  ctx.reporter.apply(FieldNotFound(field, recordType.name, recordType))
                  false
              }
            case metaTerm: MetaTerm =>
              // Record type is still a meta term, keep propagator
              false
            case fcall: FCallTerm =>
              // Record type is a function call, try to evaluate
              val evaluatedTy = summon[Reducer].reduce(fcall, ReduceMode.TypeLevel)
              if (evaluatedTy != fcall) {
                // Function call reduced, add new propagator with evaluated type
                ctx.state.addPropagator(RecordField(MetaTerm(evaluatedTy), field, fieldTy))
                true
              } else {
                // Function call couldn't be reduced, report error
                ctx.reporter.apply(NotARecordType(fcall, fcall))
                false
              }
            case ref: ReferenceCall =>
              // Record type is a reference, try to resolve
              val resolvedTy = ctx.resolve(ref)
              if (resolvedTy != ref) {
                // Reference resolved, add new propagator with resolved type
                ctx.state.addPropagator(RecordField(MetaTerm(resolvedTy), field, fieldTy))
                true
              } else {
                // Reference couldn't be resolved, report error
                ctx.reporter.apply(NotARecordType(ref, ref))
                false
              }
            case errorTerm: ErrorTerm =>
              // Record type is an error term, propagate error
              ctx.state.addPropagator(Unify(fieldTy, errorTerm))
              true
            case _ =>
              // Not a record type, report error
              ctx.reporter.apply(NotARecordType(reducedTy, reducedTy))
              false
          }
        case None =>
          // Record type not resolved yet, keep propagator
          false
      }
    }
  }

}
