package chester.scala

import scala.meta._

object Test {
  val test = q"function(argument)"
  def callit(): Unit = {
    println(test)
  }
}
