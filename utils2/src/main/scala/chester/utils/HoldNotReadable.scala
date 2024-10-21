package chester.utils

import upickle.default.*

// allow to write, not allow read
given HoldNotReadableRW: ReadWriter[HoldNotReadable[?]] =
readwriter[NotReadableRW].bimap(
  _ => NotReadableRW(),
  _ => {
    throw new UnsupportedOperationException("Cannot read HoldNotReadable")
  }
)

case class HoldNotReadable[T](inner: T) extends AnyVal

private case class NotReadableRW() derives ReadWriter
