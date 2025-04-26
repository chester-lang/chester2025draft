package chester.tyck

import chester.syntax.Name
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.uniqid.*
import chester.utils.*
import chester.utils.propagator.CommonPropagator

trait ElaboraterBase extends CommonPropagator[TyckSession] {

  object Meta {
    def rec(x: CellId[Term], default: Term)(using
        state: StateAbility[TyckSession]
    ): Term =
      state.readStable(x).getOrElse(default)

    def apply[T <: Term](x: CellId[T])(using state: StateAbility[TyckSession]): T | MetaTerm =
      state.readUnstable(x) match {
        case Some(x @ Meta(id)) => rec(id, x).asInstanceOf[T | MetaTerm]
        case Some(x)            => x
        case None               => MetaTerm(HoldNotReadable(x), meta = None)
      }

    def unapply(
        x: Term
    )(using state: StateAbility[TyckSession]): Option[CellId[Term]] = x match {
      case m: MetaTerm =>
        var result: CellId[Term] = m.unsafeRead[CellId[Term]]
        while (true)
          state.readUnstable(result) match {
            case Some(m: MetaTerm) => result = m.unsafeRead[CellId[Term]]
            case _                 => return Some(result)
          }
        throw new IllegalStateException("Unreachable")
      case _ => None
    }
  }

  def newLocalv(
      name: Name,
      ty: CellIdOr[Term],
      id: UniqidOf[LocalV],
      meta: Option[ExprMeta]
  )(using TyckSession, StateAbility[TyckSession]): LocalV = {
    val m = convertMeta(meta)
    LocalV(name, toTerm(ty), id, m)
  }

  def toTerm[T <: Term](x: CellIdOr[T])(using StateAbility[TyckSession]): T | MetaTerm = x match {
    case x: Term =>
      x match {
        case Meta(x) => Meta(x).asInstanceOf[T | MetaTerm]
        case x       => x.asInstanceOf[T | MetaTerm]
      }
    case x => Meta(x.asInstanceOf[CellId[Term]]).asInstanceOf[T | MetaTerm]
  }

  def toId[T <: Term](
      x: CellIdOr[T]
  )(using state: StateAbility[TyckSession]): CellId[T] = x match {
    case Meta(id) => id.asInstanceOf[CellId[T]]
    case x        => state.toId(x)
  }

  def merge(a: CellIdOr[Term], b: CellIdOr[Term])(using
      state: StateAbility[TyckSession],
      ab: TyckSession
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
