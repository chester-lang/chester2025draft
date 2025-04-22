package chester.utils

import java.util.function.Supplier
import java.util.concurrent.Callable

import com.eed3si9n.ifdef.*

@ifndef("jdk21")
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

@ifdef("jdk21")
class Parameter[T](default: (T | Null) = null) {
  val tl: ScopedValue[T] = ScopedValue.newInstance()

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
    ScopedValue.callWhere(tl, value, new Callable[U] {
      override def call(): U = block
    })
  }
  def get: T = tl.orElse(default.asInstanceOf[T]) match {
    case null  => throw new IllegalStateException("Value is null")
    case value => value.asInstanceOf[T]
  }
}
