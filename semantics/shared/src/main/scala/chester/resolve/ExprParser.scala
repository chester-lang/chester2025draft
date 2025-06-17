package chester.resolve

import chester.syntax.concrete.*
import chester.i18n.*
import chester.syntax.Const

import scala.util.parsing.combinator.Parsers

object ExprParser extends Parsers {
  type Elem = Expr
  def any: Parser[Expr] =
    accept(t"any expression", { case e: Expr => e })
  def id(name: String): Parser[Expr] =
    accept(t"identifier $name", { case e: Identifier if e.name == name => e })
  def caseClause(meta: Option[ExprMeta] = None): Parser[DesaltCaseClause] = id(Const.Case) ~ any ~ id(Const.Arrow2) ~ any ^^ {
    case _ ~ pattern ~ _ ~ expr => DesaltCaseClause(pattern, expr, meta = meta)
  }
}
