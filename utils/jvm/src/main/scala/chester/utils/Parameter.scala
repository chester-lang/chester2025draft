package chester.utils
import com.eed3si9n.ifdef.*

import java.util.Objects

@ifndef("jdk21")
class Parameter[T](val default: Option[T] = None) {
  private val tl: InheritableThreadLocal[T] = default match {
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

  /** Gets the current value, throwing if none is set (neither scoped nor default).
    */
  def get: T = {
    val result = tl.get()
    // Check if the result is null AND there's no default; throw if Parameter created with apply()
    if (result == null && default.isEmpty) {
      throw new IllegalStateException("No value set for Parameter (neither scoped nor default)")
    }
    // If a default exists, null means use the default. If no default, null is an error.
    // Note: requireNonNull would throw even if a default was available.
    if (result == null) default.get // Rely on initialValue or constructor for default
    else result
  }

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = {
    val result = tl.get()
    if (result == null && default.isEmpty) defaultVal // No scoped, no default param -> use arg
    else if (result == null) default.get // No scoped, has default param -> use it
    else result // Has scoped value
  }

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = {
    val result = tl.get()
    if (result == null && default.isEmpty) None // No scoped, no default
    else if (result == null) default // No scoped, has default
    else Some(result) // Has scoped
  }
}

@ifdef("jdk21")
class Parameter[T](val default: Option[T] = None) {
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

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = if (tl.isBound) tl.get() else default.getOrElse(defaultVal)

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = if (tl.isBound) Some(tl.get()) else default
}

// Add companion object with factory methods
object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
