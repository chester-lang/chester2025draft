package chester.utils

import fastparse.ParserInput

def encodeString(x: String): String = x
  .replace("\\", "\\\\")
  .replace("\n", "\\n")
  .replace("\t", "\\t")
  .replace("\r", "\\r")
  .replace("\"", "\\\"")
def parserInputToLazyList(pi: ParserInput): LazyList[String] =
  LazyList
    .from(0)
    .takeWhile(pi.isReachable)
    .map(index => pi.slice(index, index + 1))

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
