package chester.backend.scala

import scala.meta
import chester.syntax.core
import scala.meta._
import chester.syntax.core._
object Scala {
  case class ScalaContext()
  def compile(term: core.Term)(implicit ctx: ScalaContext): meta.Tree = term match {
    case IntegerTerm(i, _) => {
      require(i.isValidInt)
      Lit.Int(i.toInt)
    }
    case IntTerm(i, _)    => Lit.Int(i)
    case StringTerm(s, _) => Lit.String(s)
    case SymbolTerm(s, _) => Lit.Symbol(Symbol(s))
    case BooleanTerm(b, _) => Lit.Boolean(b)
    case UnitTerm(_) => Lit.Unit()
    case _                => throw new NotImplementedError(s"not implemented ${term.getClass.getName} $term")
  }

}
