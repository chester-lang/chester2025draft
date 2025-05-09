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
        state: StateWith[TyckSession]
    ): Term =
      state.readStable(x).getOrElse(default)

    def apply[T <: Term](x: CellId[T])(using state: StateWith[TyckSession]): T | MetaTerm =
      state.readUnstable(x) match {
        case Some(x @ Meta(id)) => rec(id, x).asInstanceOf[T | MetaTerm]
        case Some(x)            => x
        case None               => MetaTerm(HoldNotReadable(x), meta = None)
      }

    def unapply(
        x: Term
    )(using state: StateWith[TyckSession]): Option[CellId[Term]] = x match {
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

  /** Creates a new local variable with the given name, type, ID, and metadata.
    *
    * @param name
    *   The name of the local variable
    * @param ty
    *   The type of the local variable
    * @param id
    *   The unique ID for the local variable
    * @param meta
    *   Optional expression metadata
    * @return
    *   A new LocalV instance
    */
  def newLocalv(
      name: Name,
      ty: CellIdOr[Term],
      id: UniqidOf[LocalV],
      meta: Option[ExprMeta]
  )(using TyckSession, StateWith[TyckSession]): LocalV = {
    val m = convertMeta(meta)
    LocalV(name, toTerm(ty), id, m)
  }

  /** Converts a CellIdOr[T] to a Term (either the original T or a MetaTerm).
    *
    * @param x
    *   The CellIdOr[T] to convert
    * @return
    *   Either the original term or a MetaTerm
    */
  def toTerm[T <: Term](x: CellIdOr[T])(using StateWith[TyckSession]): T | MetaTerm = x match {
    case x: Term =>
      x match {
        case Meta(x) => Meta(x).asInstanceOf[T | MetaTerm]
        case x       => x.asInstanceOf[T | MetaTerm]
      }
    case x => Meta(x.asInstanceOf[CellId[Term]]).asInstanceOf[T | MetaTerm]
  }

  /** Converts a CellIdOr[T] to a CellId[T].
    *
    * @param x
    *   The CellIdOr[T] to convert
    * @return
    *   The CellId[T] for the term
    */
  def toId[T <: Term](
      x: CellIdOr[T]
  )(using state: StateWith[TyckSession]): CellId[T] = x match {
    case Meta(id) => id.asInstanceOf[CellId[T]]
    case x        => state.toId(x)
  }

  /** Merges two CellIdOr[Term] values, handling meta-terms appropriately. If the terms are identical, no action is taken.
    *
    * @param a
    *   The first term to merge
    * @param b
    *   The second term to merge
    */
  def merge(a: CellIdOr[Term], b: CellIdOr[Term])(using
                                                  state: StateWith[TyckSession],
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
