package chester.backend.scala

import scala.meta
import chester.syntax.core
import scala.meta._
import chester.syntax.core._
object Scala {
  case class ScalaContext()
  def compile(term: core.Term)(implicit ctx: ScalaContext): meta.Tree = term match {
    case IntegerTerm(i, _) => Lit.Int(i.toInt)
    case _                 => throw new NotImplementedError(s"not implemented ${term.getClass.getName} $term")
  }

}
