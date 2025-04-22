package chester.utils

import java.util.function.Supplier

class Parameter[T](default: (T | Null) = null) {
  val tl: ThreadLocal[T | Null] = ThreadLocal.withInitial(() => default)

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
    val previousValue = tl.get()
    try {
      tl.set(value)
      block
    } finally tl.set(previousValue)
  }
  def get: T = tl.get() match {
    case null  => throw new IllegalStateException("Value is null")
    case value => value.asInstanceOf[T]
  }
}
