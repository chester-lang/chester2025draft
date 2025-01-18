package chester.utils

import upickle.default._

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

case class HoldOptionNoRead[T](inner: Option[T]) extends AnyVal {
  def get: T = inner.get
}

given HoldOptionNoReadRW[T]: ReadWriter[HoldOptionNoRead[T]] =
  readwriter[NotReadableRW].bimap(
    _ => NotReadableRW(),
    _ => {
      HoldOptionNoRead(None)
    }
  )
