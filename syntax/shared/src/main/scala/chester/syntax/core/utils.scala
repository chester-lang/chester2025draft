package chester.syntax.core

import cats.data.*
import chester.syntax.*
import chester.uniqid.*
import chester.utils.*

import scala.language.implicitConversions

// Referencing Setω in Agda
val Typeω: Type = Type(LevelUnrestricted(None), meta = None)

def UnitType(meta: Option[TermMeta]): TupleType =
  TupleType(Vector.empty, meta = meta)

object UnitTerm_ {
  def unapply(x: Any): Option[Option[TermMeta]] = PartialFunction.condOpt(x) { case TupleTerm(Vector(), meta) =>
    meta
  }

  def apply(meta: Option[TermMeta]): TupleTerm =
    TupleTerm(Vector.empty, meta = meta)

}

object AbstractIntTerm_ {
  def from(value: BigInt, meta: Option[TermMeta]): AbstractIntTerm =
    if (value.isValidInt) IntTerm(value.asInt, meta)
    else IntegerTerm(value, meta)

  def unapply(term: Term): Option[BigInt] = PartialFunction.condOpt(term) {
    case IntTerm(value, _)     => BigInt(value)
    case IntegerTerm(value, _) => value
  }
}

object Bind_ {
  @deprecated("meta")
  def from(bind: LocalVar): Bind = Bind(bind, bind.ty, None)
}

object MetaTerm_ {
  @deprecated("meta")
  def from[T](x: T): MetaTerm[?] = MetaTerm(HoldNotReadable(x), meta = None)
}

object FunctionType_ {
  @deprecated("meta")
  def apply(telescope: TelescopeTerm, resultTy: Term): FunctionType =
    FunctionType(Vector(telescope), resultTy, meta = None)
}

def TyToty: FunctionType = {
  val ty = LocalVar("x", Type0, Uniqid.generate[LocalVar], None)
  FunctionType_(TelescopeTerm_.from(ArgTerm_.from(ty)), ty)
}

object Intersection_ {
  @deprecated("meta")
  def from(xs: Vector[Term]): Term = {
    val flattened = xs.flatMap {
      case Intersection(ys, _) => ys
      case x                   => Vector(x)
    }.distinct
    if (flattened.size == 1) return flattened.head
    Intersection(flattened.assumeNonEmpty, None)
  }
}

object Union_ {
  @deprecated("meta")
  def from(xs: Vector[Term]): Term = {
    val flattened = xs
      .flatMap {
        case Union(ys, _) => ys
        case x            => Vector(x)
      }
      .distinct
      .filterNot(x => x.isInstanceOf[NothingType])
    if (flattened.size == 1) return flattened.head
    if (flattened.nonEmpty) Union(flattened.assumeNonEmpty, None)
    else NothingType(None)
  }
}

object ArgTerm_ {

  @deprecated("meta")
  def from(bind: LocalVar): ArgTerm = ArgTerm(Some(bind), bind.ty, meta = None)
}

object TelescopeTerm_ {
  @deprecated("meta")
  def from(x: ArgTerm*): TelescopeTerm = TelescopeTerm(x.toVector, meta = None)
}
