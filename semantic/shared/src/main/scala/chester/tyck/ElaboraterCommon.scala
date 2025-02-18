package chester.tyck

import chester.error.*
import chester.resolve.{SimpleDesalt, resolveOpSeq}
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.*
import chester.utils.propagator.*
import scala.util.boundary
import scala.util.boundary.break
import scala.language.implicitConversions

trait ElaboraterCommon extends ProvideCtx with ElaboraterBase with CommonPropagator[Tyck] {

  trait EffectsCell extends Cell[Effects] {
    def requireEffect(
        effect: Term
    )(using Tyck, StateAbility[Tyck]): LocalV = {
      ???
    }
  }

  def toEffectsM(
      x: CellIdOr[Effects]
  )(using StateAbility[Tyck]): EffectsM = x match {
    case x: Effects     => x
    case x: EffectsCell => Meta(x.asInstanceOf[CellId[Effects]])
    case _              => unreachable()
  }

  def toEffectsCell(
      x: EffectsM
  )(using state: StateAbility[Tyck]): CIdOf[EffectsCell] = x match {
    case x: Effects => state.addCell(FixedEffectsCell(x))
    case Meta(x)    => x.asInstanceOf[CIdOf[EffectsCell]]
    case _          => unreachable()
  }

  case class DynamicEffectsCell(effects: Map[LocalV, Term] = Map.empty)
      extends BaseMapCell[LocalV, Term]
      with EffectsCell
      with UnstableCell[Effects]
      with NoFill[Effects] {
    override def add(key: LocalV, value: Term): DynamicEffectsCell = {
      require(!effects.contains(key))
      copy(effects = effects.updated(key, value))
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

  def newMeta(using ck: Tyck, state: StateAbility[Tyck]): CellId[Term] = {
    val cell = state.addCell(OnceCell[Term]())
    cell
  }

  def newType(using ck: Tyck, state: StateAbility[Tyck]): CellId[Term] = {
    val cell = state.addCell(OnceCell[Term](default = Some(AnyType0)))
    cell
  }

  def newTypeTerm(using Tyck, StateAbility[Tyck]): Term = {
    Meta(newType)
  }

  def newEffects(using
      ck: Tyck,
      state: StateAbility[Tyck]
  ): CIdOf[EffectsCell] = {
    val cell = state.addCell(DynamicEffectsCell())
    cell
  }

  def newEffectsTerm(using Tyck, StateAbility[Tyck]): Effects | MetaTerm = {
    Meta(newEffects)
  }

  def readVar(
      x: Term
  )(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): Term = {
    boundary {
      var result = x
      while (true) {
        result match {
          case varCall: ReferenceCall =>
            localCtx.getKnown(varCall) match {
              case Some(tyAndVal) =>
                state.readStable(tyAndVal.valueId) match {
                  case Some(value) => result = value
                  case None => break(result)
                }
              case None => break(result)
            }
          case _ => break(result)
        }
      }
      result
    }
  }

  def readMetaVar(
      x: Term
  )(using localCtx: Context, ck: Tyck, state: StateAbility[Tyck]): Term = {
    boundary {
      var result = x
      while (true) {
        result match {
          case varCall: ReferenceCall =>
            localCtx.getKnown(varCall) match {
              case Some(tyAndVal) =>
                state.readStable(tyAndVal.valueId) match {
                  case Some(value) => result = value
                  case None => break(result)
                }
              case None => break(result)
            }
          case Meta(id) =>
            state.readStable(id) match {
              case Some(x) => result = x
              case None    => break(result)
            }
          case _ => break(result)
        }
      }
      result
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
