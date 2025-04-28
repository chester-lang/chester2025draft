package chester.utils

class MutBox[T](var value: T) {
  def get: T = value

  def set(x: T): Unit = value = x

  def update(f: T => T): Unit = value = f(value)

  def updateAndMap[U](f: T => (T, U)): U = {
    val (newValue, result) = f(value)
    value = newValue
    result
  }
}
