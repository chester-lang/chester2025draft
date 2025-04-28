package chester.utils.impls

import spire.math.UInt
import upickle.default.*
import chester.utils.asInt

// Provide a ReadWriter for spire.math.UInt by converting to/from Int
implicit val uintRW: ReadWriter[UInt] =
  readwriter[Int].bimap(_.asInt, UInt(_))
