package chester.utils
import com.eed3si9n.ifdef.*

@ifndef("jdk21")
final class Parameter[T]( default: Option[T] = None) {
  private val tl: InheritableThreadLocal[T] = default match {
    case Some(value) =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = value
      }
    case None =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = throw new IllegalStateException("No value set for Parameter")
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

@ifdef("jdk21")
final class Parameter[T](default: Option[T] = None) {
  private val tl: ScopedValue[T] = ScopedValue.newInstance()

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
    ScopedValue.callWhere(
      tl,
      value,
      () => block
    )
  }

  /** Gets the current value, throwing if none is set (neither scoped nor default).
    */
  def get: T = if (tl.isBound) tl.get() else default.getOrElse(throw new IllegalStateException("No value set for Parameter"))
}

// Add companion object with factory methods
object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
