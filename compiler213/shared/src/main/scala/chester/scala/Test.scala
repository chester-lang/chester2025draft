package chester.scala

import chester.backend.scala.Scala

import scala.meta


import chester.syntax.core.simple._
object Test {
  val test: meta.Term.Apply = q"function(argument)"
  def callit(): Unit = {
    println(test)
    println(Scala.compileExpr(BooleanTerm(false, meta = None)))
  }
}
