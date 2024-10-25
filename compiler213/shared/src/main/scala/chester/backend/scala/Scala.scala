package chester.backend.scala

import scala.meta
import chester.syntax.core
import scala.meta._
import chester.syntax.core._
object Scala {
  case class ScalaContext()
  def compileExpr(term: core.Term)(implicit ctx: ScalaContext = null): meta.Tree = term match {
    case IntegerTerm(i, _) => {
      require(i.isValidInt)
      Lit.Int(i.toInt)
    }
    case IntTerm(i, _)     => Lit.Int(i)
    case StringTerm(s, _)  => Lit.String(s)
    case SymbolTerm(s, _)  => Lit.Symbol(Symbol(s))
    case BooleanTerm(b, _) => Lit.Boolean(b)
    case UnitTerm(_)       => Lit.Unit()
    case _                 => throw new NotImplementedError(s"not implemented ${term.getClass.getName} $term")
  }
  def compileStmt(stmt: core.StmtTerm)(implicit ctx: ScalaContext = null): meta.Stat = stmt match {
    case _ => throw new NotImplementedError(s"not implemented ${stmt.getClass.getName} $stmt")
  }
}
