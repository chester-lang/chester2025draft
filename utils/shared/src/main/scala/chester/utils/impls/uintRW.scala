package chester.utils.impls

import spire.math.UInt
import upickle.default.*

// Provide a ReadWriter for spire.math.UInt by converting to/from Int
implicit val uintRW: ReadWriter[UInt] =
  readwriter[Int].bimap(_.toInt, UInt(_)) 