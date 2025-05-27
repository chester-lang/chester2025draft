package chester.tyck

import chester.cell.*
import chester.error.*
import chester.syntax.core.*
import chester.syntax.Name
import chester.uniqid.Uniqid
import chester.utils.*
import chester.utils.propagator.*
import chester.utils.cell.*

import scala.util.boundary
import scala.util.boundary.break
import scala.language.implicitConversions

trait ElaboraterCommon extends ProvideContextOps with ElaboraterBase with CommonPropagator[TyckOps] {

  extension (eff: EffectsCellContent) {

    def requireEffect(
        effect: Term
    )(using ck: TyckOps, state: StateOps[TyckOps]): LocalV = {
      // Check if this effect already exists in the cell
      val currentEffects = eff.readUnstable.map(_.effects).getOrElse(Map.empty)

      // Try to find an existing entry for this effect
      val existingKey = currentEffects
        .find { case (_, existingEffect) =>
          // Simple equality check for now
          existingEffect == effect
        }
        .map(_._1)

      // Return existing key or create a new one
      existingKey.getOrElse {
        // Create a new key for this effect with proper type
        val id = Uniqid.generate[LocalV]
        val newKey = LocalV(Name("effect"), AnyType0, id, None)

        // Add the effect to the cell
        eff match {
          case cell: DynamicEffectsCellContent =>
            val updatedCell = cell.add(newKey, effect)
            state.fill(eff.asInstanceOf[CellId[Effects]], updatedCell.asInstanceOf[Effects])
          case _ =>
            // For other cell types, we may need different handling
            ck.reporter.report(CannotAddEffectError(effect))
        }

        newKey
      }
    }
  }

  def toEffectsM(
      x: CellIdOr[Effects]
  )(using StateOps[TyckOps]): EffectsM = x match {
    case x: Effects            => x
    case x: EffectsCellContent => Meta(x.asInstanceOf[CellId[Effects]])
    case _                     => unreachable()
  }

  def toEffectsCell(
      x: EffectsM
  )(using state: StateOps[TyckOps]): CIdOf[EffectsCellContent] = x match {
    case x: Effects => state.addCell(FixedEffectsCellContent(x))
    case Meta(x)    => x.asInstanceOf[CIdOf[EffectsCellContent]]
    case _          => unreachable()
  }

  def newMeta(using _ck: TyckOps, state: StateOps[TyckOps]): CellId[Term] = {
    val cell = state.addCell(OnceCellContent[Term]())
    cell
  }

  def newType(using ck: TyckOps, state: StateOps[TyckOps]): CellId[Term] = {
    val cell = state.addCell(OnceCellContent[Term](default = Some(AnyType0)))
    cell
  }

  def newTypeTerm(using TyckOps, StateOps[TyckOps]): Term =
    Meta(newType)

  def newEffects(using
      _ck: TyckOps,
      state: StateOps[TyckOps]
  ): CIdOf[EffectsCellContent] = {
    val cell = state.addCell(DynamicEffectsCellContent())
    cell
  }

  def newEffectsTerm(using TyckOps, StateOps[TyckOps]): Effects | MetaTerm =
    Meta(newEffects)

  def readVar(
      x: Term
  )(using localCtx: Context, _ck: TyckOps, state: StateOps[TyckOps]): Term =
    boundary {
      var result = x
      while (true)
        result match {
          case varCall: ReferenceCall =>
            localCtx.getKnown(varCall) match {
              case Some(tyAndVal) =>
                state.readStable(tyAndVal.valueId) match {
                  case Some(value) => result = value
                  case None        => break(result)
                }
              case None => break(result)
            }
          case _ => break(result)
        }
      result
    }

  def readMetaVar(
      x: Term
  )(using localCtx: Context, _ck: TyckOps, state: StateOps[TyckOps]): Term =
    boundary {
      var result = x
      while (true)
        result match {
          case varCall: ReferenceCall =>
            localCtx.getKnown(varCall) match {
              case Some(tyAndVal) =>
                state.readStable(tyAndVal.valueId) match {
                  case Some(value) => result = value
                  case None        => break(result)
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
      result
    }

  @silentDeprecated("see newer one in chester.elab")
  class MutableContext(var ctx: Context) {
    def update(f: Context => Context): Unit =
      ctx = f(ctx)
  }

  given mutL(using m: MutableContext): Context = m.ctx
  implicit def mutLc(m: MutableContext): Context = m.ctx

}
