package chester.tyck

import chester.error.*
import chester.resolve.{SimpleDesalt, resolveOpSeq}
import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.syntax.core.orm.EffectsM
import chester.utils.*
import chester.utils.propagator.CommonPropagator
import chester.uniqid.*

trait ElaboraterCommon extends ProvideCtx with ElaboraterBase with CommonPropagator[Tyck] {

  trait EffectsCell extends Cell[Effects] {
    def requireEffect(
        effect: Term
    )(using ck: Tyck, state: StateAbility[Tyck]): LocalV = {
      ???
    }
  }

  def toEffectsM(
      x: CellIdOr[Effects]
  )(using state: StateAbility[Tyck]): EffectsM = x match {
    case x: Effects     => x
    case x: EffectsCell => Meta(x.asInstanceOf[CellId[Effects]])
  }

  def toEffectsCell(
      x: EffectsM
  )(using state: StateAbility[Tyck]): CIdOf[EffectsCell] = x match {
    case x: Effects => state.addCell(FixedEffectsCell(x))
    case Meta(x)    => x.asInstanceOf[CIdOf[EffectsCell]]
  }

  case class DynamicEffectsCell(effects: Map[LocalV, Term] = Map())
      extends BaseMapCell[LocalV, Term]
      with EffectsCell
      with UnstableCell[Effects]
      with NoFill[Effects] {
    override def add(key: LocalV, value: Term): DynamicEffectsCell = {
      require(!effects.contains(key))
      copy(effects = effects + (key -> value))
    }

    override def readUnstable: Option[Effects] = Some(Effects(effects, None))
  }

  case class FixedEffectsCell(effects: Effects) extends EffectsCell with NoFill[Effects] {
    override def readStable: Option[Effects] = Some(effects)
  }

  def resolve(
      expr: Expr
  )(using localCtx: Context, reporter: Reporter[TyckProblem]): Expr = {
    val result = SimpleDesalt.desugarUnwrap(expr) match {
      case opseq: OpSeq => {
        val result = resolveOpSeq(reporter, localCtx.operators, opseq)
        result
      }
      case default => default
    }
    reuse(expr, result)
  }

  type Literals = Expr & (IntegerLiteral | RationalLiteral | StringLiteral | SymbolLiteral)

  case class Unify(lhs: CellId[Term], rhs: CellId[Term], cause: Expr)(using
      localCtx: Context
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
  )(using localCtx: Context)
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
            val unionType = Union.from(rhsValues.assumeNonEmpty)
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

      case (Union(_, _), Union(_, _)) => ???

      case (Intersection(_, _), Intersection(_, _)) => ???

      case _ => false
    }
  }

  case class LiteralType(x: Literals, tyLhs: CellId[Term])(using
      localCtx: Context
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

  def newMeta(using ck: Tyck, state: StateAbility[Tyck]): CellId[Term] = {
    val cell = state.addCell(OnceCell[Term]())
    cell
  }

  def newType(using ck: Tyck, state: StateAbility[Tyck]): CellId[Term] = {
    val cell = state.addCell(OnceCell[Term](default = Some(AnyType0Debug)))
    cell
  }

  def newTypeTerm(using ck: Tyck, state: StateAbility[Tyck]): Term = {
    Meta(newType)
  }

  def newEffects(using
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CIdOf[EffectsCell] = {
    val cell = state.addCell(DynamicEffectsCell())
    cell
  }

  def newEffectsTerm(using ck: Tyck, state: StateAbility[Tyck]): Effects | MetaTerm = {
    Meta(newEffects)
  }

  def readVar(
      x: Term
  )(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): Term = {
    var result = x
    while (true) {
      result match {
        case varCall: ReferenceCall =>
          localCtx.getKnown(varCall) match {
            case Some(tyAndVal) =>
              result = state.readStable(tyAndVal.valueId).getOrElse {
                return result
              }
            case None => return result
          }
        case _ => return result
      }
    }
    result
  }

  def readMetaVar(
      x: Term
  )(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): Term = {
    var result = x
    while (true) {
      result match {
        case varCall: ReferenceCall =>
          localCtx.getKnown(varCall) match {
            case Some(tyAndVal) =>
              result = state.readStable(tyAndVal.valueId).getOrElse {
                return result
              }
            case None => return result
          }
        case Meta(id) =>
          state.readStable(id) match {
            case Some(x) => result = x
            case None    => return result
          }
        case _ => return result
      }
    }
    result
  }

  def unify(lhs: Term, rhs: Term, cause: Expr)(using
      localCtx: Context,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): Unit = {
    if (lhs == rhs) return
    val lhsResolved = readVar(lhs)
    val rhsResolved = readVar(rhs)
    if (lhsResolved == rhsResolved) return
    (lhsResolved, rhsResolved) match {
      case (Meta(lhs), rhs) => unify(lhs, rhs, cause)
      case (lhs, Meta(rhs)) => unify(lhs, rhs, cause)

      // Structural unification for ListType
      case (ListType(elem1, _), ListType(elem2, _)) =>
        unify(elem1, elem2, cause)

      case (Type(LevelUnrestricted(_), _), Type(LevelFinite(_, _), _)) => ()

      case (x, Intersection(xs, _)) =>
        if (xs.exists(tryUnify(x, _))) return
        ck.reporter.apply(TypeMismatch(lhs, rhs, cause)) // TODO

      // Structural unification for TupleType
      case (TupleType(types1, _), TupleType(types2, _)) if types1.length == types2.length =>
        types1.zip(types2).foreach { case (t1, t2) =>
          unify(t1, t2, cause)
        }

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
      if (!t1.isDefined) return ZonkResult.Require(Vector(this.tRhs))
      val ty = t1.get
      assert(listT1.isEmpty)
      state.fill(this.listTLhs, ListType(ty, meta = None))
      ZonkResult.Done
    }
  }

  class MutableContext(var ctx: Context) {
    def update(f: Context => Context): Unit = {
      ctx = f(ctx)
    }
  }

  given mutL(using m: MutableContext): Context = m.ctx
  implicit def mutLc(m: MutableContext): Context = m.ctx

  def generateImplicitArg(
      paramTy: Term,
      cause: Expr
  )(using
      ctx: Context,
      state: StateAbility[Tyck],
      ck: Tyck
  ): Term = {
    // TODO: Implement logic to generate an implicit argument based on the parameter type.
    // This could involve looking up available implicit values in the context.
    // For now, create a new meta-term as a placeholder.
    val argTerm = newMeta
    // Report a warning or note that an implicit argument is not provided explicitly.
    ck.reporter(MissingImplicitArgumentWarning(paramTy, cause))
    toTerm(argTerm)
  }

}

trait ElaboraterBase extends CommonPropagator[Tyck] {

  object Meta {
    def rec(x: CellId[Term], default: Term)(using
        state: StateAbility[Tyck]
    ): Term = {
      state.readStable(x) match {
        case Some(x) => x
        case None    => default
      }
    }

    def apply[T <: Term](x: CellId[T])(using state: StateAbility[Tyck]): T | MetaTerm = {
      state.readUnstable(x) match {
        case Some(x @ Meta(id)) => rec(id, x).asInstanceOf[T | MetaTerm]
        case Some(x)            => x
        case None               => MetaTerm.from(x)
      }
    }

    def unapply(
        x: Term
    )(using state: StateAbility[Tyck]): Option[CellId[Term]] = x match {
      case m: MetaTerm => {
        var result: CellId[Term] = m.unsafeRead[CellId[Term]]
        while (true) {
          state.readUnstable(result) match {
            case Some(m: MetaTerm) => result = m.unsafeRead[CellId[Term]]
            case _                 => return Some(result)
          }
        }
        throw new IllegalStateException("Unreachable")
      }
      case _ => None
    }
  }

  def newLocalv(
      name: Name,
      ty: CellIdOr[Term],
      id: UniqidOf[LocalV],
      meta: Option[ExprMeta]
  )(using ck: Tyck, state: StateAbility[Tyck]): LocalV = {
    val m = convertMeta(meta)
    LocalV(name, toTerm(ty), id, m)
  }

  def toTerm[T <: Term](x: CellIdOr[T])(using state: StateAbility[Tyck]): T | MetaTerm = x match {
    case x: Term =>
      x match {
        case Meta(x) => Meta(x).asInstanceOf[T | MetaTerm]
        case x       => x.asInstanceOf[T | MetaTerm]
      }
    case x => Meta(x.asInstanceOf[CellId[Term]]).asInstanceOf[T | MetaTerm]
  }

  def toId[T <: Term](
      x: CellIdOr[T]
  )(using state: StateAbility[Tyck]): CellId[T] = x match {
    case Meta(id) => id.asInstanceOf[CellId[T]]
    case x        => state.toId(x)
  }

  def merge(a: CellIdOr[Term], b: CellIdOr[Term])(using
      state: StateAbility[Tyck],
      ab: Tyck
  ): Unit = {
    if (a == b) return
    val t1 = toTerm(a)
    val t2 = toTerm(b)
    if (a == b) return
    (t1, t2) match {
      case (Meta(t1), t2) => state.fill(t1, t2)
      case (t1, Meta(t2)) => state.fill(t2, t1)
      case _              => ???
    }
  }
}
