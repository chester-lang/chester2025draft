package chester.utils.impls

import spire.math.Rational
import upickle.default._

case class RationalSerialized(numerator: BigInt, denominator: BigInt) derives ReadWriter {
  def toRational: Rational = Rational(numerator, denominator)
}

extension (r: Rational) {
  def toSerialized: RationalSerialized =
    RationalSerialized(r.numerator.toBigInt, r.denominator.toBigInt)
}

implicit val rationalRW: ReadWriter[Rational] =
  readwriter[RationalSerialized].bimap(_.toSerialized, _.toRational)
