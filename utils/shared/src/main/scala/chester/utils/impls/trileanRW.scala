package chester.utils.impls

import upickle.default._
import spire.math.Trilean
private enum Three derives ReadWriter {
  case True, False, Unknown
}

private def threeToTrilean(three: Three): Trilean = three match {
  case Three.True    => Trilean.True
  case Three.False   => Trilean.False
  case Three.Unknown => Trilean.Unknown
}

private def trileanToThree(trilean: Trilean): Three = trilean match {
  case Trilean.True    => Three.True
  case Trilean.False   => Three.False
  case Trilean.Unknown => Three.Unknown
}

implicit val trileanRW: ReadWriter[Trilean] =
  readwriter[Three].bimap(trileanToThree, threeToTrilean)
