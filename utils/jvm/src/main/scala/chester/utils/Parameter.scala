package chester.utils
import com.eed3si9n.ifdef.*

import java.util.Objects

@ifndef("jdk21")
class Parameter[T](default: Option[T] = None) {
  val tl: InheritableThreadLocal[T] = default match {
    case Some(value) =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = value
      }
    case None => new InheritableThreadLocal[T]()
  }

  def withValue[U](value: T)(block: => U): U = {
    Objects.requireNonNull(value)
    val previousValue = tl.get()
    try {
      tl.set(value)
      block
    } finally tl.set(previousValue)
  }
  def get: T = {
    val result = tl.get()
    Objects.requireNonNull(result)
    result
  }
}

@ifdef("jdk21")
class Parameter[T](default: Option[T] = None) {
  val tl: ScopedValue[T] = ScopedValue.newInstance()

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
    ScopedValue.callWhere(
      tl,
      value,
      new java.util.concurrent.Callable[U] {
        override def call(): U = block
      }
    )
  }
  def get: T = if (tl.isBound()) tl.get() else default.getOrElse(throw new IllegalStateException("No default value"))
}
