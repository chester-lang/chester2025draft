package chester.error

trait Reporter[-T] {
  def report(value: T): Unit
}

object Reporter {
  def report[T](value: T)(using reporter: Reporter[T]): Unit =
    reporter.report(value)
}

object StdErrReporter extends Reporter[Problem] {
  def report(value: Problem): Unit =
    println(value)
}

extension [T](reporter: Reporter[T]) {
  def report(xs: Seq[T]): Unit = xs.foreach(reporter.report)
}

class VectorReporter[T] extends Reporter[T] {
  private val buffer = scala.collection.mutable.ArrayBuffer[T]()

  def report(value: T): Unit = this.synchronized(buffer += value): Unit

  def getReports: Vector[T] = this.synchronized(buffer.toVector)
}
