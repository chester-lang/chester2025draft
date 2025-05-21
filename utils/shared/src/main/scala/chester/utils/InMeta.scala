package chester.utils

import upickle.default.*

// allow to write, not allow read
given HoldNotReadableRW: ReadWriter[InMeta[?]] =
  readwriter[InMetaRW].bimap(
    _ => InMetaRW(),
    _ => throw new UnsupportedOperationException("Cannot read HoldNotReadable")
  )

case class InMeta[T](inner: T) extends AnyVal {
  override def equals(obj: Any): Boolean = {
    obj match {
      case InMeta(other) => inner == other
      case _             => false
    }
  }
}

private case class InMetaRW() derives ReadWriter

case class HoldOptionNoRead[T](inner: Option[T]) extends AnyVal {
  def get: T = inner.get
}

given HoldOptionNoReadRW[T]: ReadWriter[HoldOptionNoRead[T]] =
  readwriter[InMetaRW].bimap(
    _ => InMetaRW(),
    _ => HoldOptionNoRead(None)
  )
