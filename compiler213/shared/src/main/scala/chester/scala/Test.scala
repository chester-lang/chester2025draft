package chester.scala

import chester.backend.scala.Scala

import scala.meta
import chester.syntax.core

import scala.meta._
import chester.syntax.core._

import scala.meta._

object Test {
  val test: meta.Term.Apply = q"function(argument)"
  def callit(): Unit = {
    println(test)
    println(Scala.compile(core.BooleanTerm(false, meta = None)))
  }
}
