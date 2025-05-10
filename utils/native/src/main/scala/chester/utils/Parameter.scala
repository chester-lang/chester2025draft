package chester.utils
import com.eed3si9n.ifdef.*

@ifndef("scalaNativeNoMultithread")
final class Parameter[T]( default: Option[T] = None) {
  private val tl: InheritableThreadLocal[T] = default match {
    case Some(value) =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = value
      }
    case None =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = throw new IllegalStateException("No default value set for Parameter")
      }
  }

  def withValue[U](value: T)(block: => U): U = {
    val previousValue = tl.get()
    try {
      tl.set(value)
      block
    } finally tl.set(previousValue)
  }

  /** Gets the current value, throwing if none is set (neither scoped nor default).
   */
  def get: T = {
    tl.get()
  }
}

@ifdef("scalaNativeNoMultithread")
final class Parameter[T](default: Option[T] = None) {
  private var tl: Option[T] = default

  def withValue[U](value: T)(block: => U): U = {
    val previousValue = tl
    try {
      tl = Some(value)
      block
    } finally tl = previousValue
  }

  /** Gets the current value, throwing if none is set (neither scoped nor default).
   */
  def get: T = tl.getOrElse(throw new IllegalStateException("No value set for Parameter"))
}


// Add companion object with factory methods
object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
