package chester.utils
import com.eed3si9n.ifdef.*
@ifndef("scalaNativeNoMultithread")
class Parameter[T](val default: Option[T] = None) {
  val tl: InheritableThreadLocal[T] = default match {
    case Some(value) =>
      new InheritableThreadLocal[T] {
        override def initialValue(): T = value
      }
    case None => new InheritableThreadLocal[T]()
  }

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
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
    if (result == null && default.isEmpty) {
      throw new IllegalStateException("No value set for Parameter (neither scoped nor default)")
    }
    if (result == null) default.get
    else result
  }

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = {
    val result = tl.get()
    if (result == null && default.isEmpty) defaultVal
    else if (result == null) default.get
    else result
  }

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = {
    val result = tl.get()
    if (result == null && default.isEmpty) None
    else if (result == null) default
    else Some(result)
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

  /** Gets the current value, throwing if none is set (neither scoped nor default).
    */
  def get: T = tl.getOrElse(throw new IllegalStateException("No value set for Parameter"))

  /** Gets the current value, or returns the provided default if none is set.
    */
  def getOrElse(defaultVal: => T): T = tl.getOrElse(defaultVal)

  /** Gets the current value as an Option.
    */
  def getOption: Option[T] = tl
}

// Add companion object with factory methods
object Parameter {
  def apply[T](): Parameter[T] = new Parameter(None)
  def withDefault[T](default: T): Parameter[T] = new Parameter(Some(default))
}
