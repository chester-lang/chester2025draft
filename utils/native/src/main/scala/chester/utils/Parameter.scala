package chester.utils

import java.util.function.Supplier
import java.util.Objects

import com.eed3si9n.ifdef.*
@ifndef("scalaNativeNoMultithread")
class Parameter[T](default: Option[T] = None) {
  val tl: InheritableThreadLocal[T] = default match {
    case Some(value) => new InheritableThreadLocal[T] {
      override def initialValue(): T = value
    }
    case None      => new InheritableThreadLocal[T]()
  }

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
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

@ifdef("scalaNativeNoMultithread")
class Parameter[T](default: Option[T] = None) {
  var tl: Option[T] = default

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
