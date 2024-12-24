package chester.syntax.core

import chester.doc.*
import chester.error.*
import chester.error.ProblemUpickle.*
import chester.utils.given
import chester.utils.impls.*
import upickle.default.*

import scala.language.implicitConversions
import cats.data.*
import chester.doc.const.*
import chester.error.Problem
import chester.syntax.*
import chester.syntax.core.spec.spec.*
import chester.uniqid.*
import chester.utils.*
import chester.utils.doc.*
import spire.math.*

import scala.collection.immutable.HashMap

// Referencing Setω in Agda
val Typeω: Type = Type(LevelUnrestricted(None), meta = None)

def UnitType(meta: OptionTermMeta): TupleType =
  TupleType(Vector.empty, meta = meta)

object UnitTerm_ {
  def unapply(x: Any): Option[OptionTermMeta] = x match {
    case TupleTerm(Vector(), meta) => Some(meta)
    case _ => None
  }

  def apply(meta: OptionTermMeta): TupleTerm =
    TupleTerm(Vector.empty, meta = meta)

}

object Effects_ {
  val Empty: Effects = Effects(HashMap.empty, meta = None)
}

val NoEffect = Effects_.Empty

object AbstractIntTerm_ {
  def from(value: BigInt, meta: OptionTermMeta): AbstractIntTerm =
    if (value.isValidInt) IntTerm(value.toInt, meta)
    else IntegerTerm(value, meta)

  def unapply(term: Term): Option[BigInt] = term match {
    case IntTerm(value, _) => Some(BigInt(value))
    case IntegerTerm(value, _) => Some(value)
    case _ => None
  }
}

object NaturalTerm {

  @deprecated("meta")
  def apply(value: BigInt): AbstractIntTerm = AbstractIntTerm_.from(value, meta = None)
}

object Bind_ {
  @deprecated("meta")
  def from(bind: LocalV): Bind = Bind(bind, bind.ty, None)
}

object MetaTerm_ {
  @deprecated("meta")
  def from[T](x: T): MetaTerm = MetaTerm(HoldNotReadable(x), meta = None)
}

object FunctionType_ {
  @deprecated("meta")
  def apply(telescope: TelescopeTerm, resultTy: Term): FunctionType = {
    new FunctionType(Vector(telescope), resultTy, meta = None)
  }
}

def TyToty: FunctionType = {
  val ty = LocalV("x", Type0, Uniqid.generate[LocalV], None)
  FunctionType_(TelescopeTerm_.from(ArgTerm_.from(ty)), ty)
}

object Intersection_ {
  @deprecated("meta")
  def from(xs: Vector[Term]): Term = {
    val flattened = xs.flatMap {
      case Intersection(ys, _) => ys
      case x => Vector(x)
    }.distinct
    if (flattened.size == 1) return flattened.head
    new Intersection(flattened.assumeNonEmpty, None)
  }
}

object Union_ {
  @deprecated("meta")
  def from(xs: Vector[Term]): Term = {
    val flattened = xs
      .flatMap {
        case Union(ys, _) => ys
        case x => Vector(x)
      }
      .distinct
      .filter(x => !x.isInstanceOf[NothingType])
    if (flattened.size == 1) return flattened.head
    if (flattened.nonEmpty) new Union(flattened.assumeNonEmpty, None)
    else NothingType(None)
  }
}

object ArgTerm_ {

  @deprecated("meta")
  def from(bind: LocalV): ArgTerm = ArgTerm(bind, bind.ty, meta = None)
}

object TelescopeTerm_ {
  @deprecated("meta")
  def from(x: ArgTerm*): TelescopeTerm = TelescopeTerm(x.toVector, meta = None)
}
