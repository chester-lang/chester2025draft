package chester.utils.impls

import spire.math.Natural
import upickle.default.*

// Provide a ReadWriter for spire.math.Natural by converting to/from BigInt
implicit val naturalRW: ReadWriter[Natural] =
  readwriter[BigInt].bimap(_.toBigInt, Natural(_))
