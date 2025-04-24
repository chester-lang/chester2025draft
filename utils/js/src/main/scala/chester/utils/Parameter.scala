package chester.utils

import java.util.Objects

class Parameter[T](default: Option[T] = None) {
  private var tl: Option[T] = default

  def withValue[U](value: T)(block: => U): U = {
    Objects.requireNonNull(value)
    val previousValue = tl
    try {
      tl = Some(value)
      block
    } finally tl = previousValue
  }
  def get: T = tl.getOrElse(throw new IllegalStateException("No default value"))
}
