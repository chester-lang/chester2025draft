package chester.utils

import chester.utils.asInt
import spire.math.UInt
import upickle.default.*

// Provide a ReadWriter for spire.math.UInt by converting to/from Int
given uintRW: ReadWriter[UInt] =
  readwriter[Int].bimap(_.asInt, UInt(_))
