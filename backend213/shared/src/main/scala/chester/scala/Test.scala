package chester.scala
import scala.meta
import scala.meta._
import scala.annotation.experimental
@experimental
object Test {
  val test: meta.Term.Apply = q"function(argument)"
  def callit(): Unit =
    println(test)
}
