package chester.utils

import spire.math.Rational
import upickle.default.*

private case class RationalSerialized(numerator: BigInt, denominator: BigInt) derives ReadWriter {
  def toRational: Rational = Rational(numerator, denominator)
}

extension (r: Rational) {
  private[utils] def toSerialized: RationalSerialized =
    RationalSerialized(r.numerator.toBigInt, r.denominator.toBigInt)
}

given rationalRW: ReadWriter[Rational] =
  readwriter[RationalSerialized].bimap(_.toSerialized, _.toRational)
