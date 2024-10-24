package chester.scala

import scala.meta._

object Test {
  val test: Term.Apply = q"function(argument)"
  def callit(): Unit = {
    println(test)
  }
}
