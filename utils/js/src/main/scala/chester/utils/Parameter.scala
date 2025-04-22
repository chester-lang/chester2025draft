package chester.utils

class Parameter[T](default: T | Null = null) {
  var tl: T | Null = default

  def withValue[U](value: T)(block: => U): U = {
    require(value != null)
    val previousValue = tl
    try {
      tl = value
      block
    } finally tl = previousValue
  }
  def get: T = tl match {
    case null  => throw new IllegalStateException("Value is null")
    case value => value.asInstanceOf[T]
  }
}
