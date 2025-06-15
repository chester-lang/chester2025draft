package chester.resolve

import chester.reader.ParseError
import chester.syntax.concrete.Expr

type ExprsMatcher = Function1[Seq[Expr], Boolean]

case class ExprsParser[+A](parser: PartialFunction[Seq[Expr], A], prettyError: Option[Function1[Seq[Expr], ParseError]] = None)
type ExprsParserAny = ExprsParser[Any]

object Exprs {
  def parse(exprs: Seq[Expr], parsers: Seq[ExprsParser[Any]]): Either[Option[ParseError], Seq[Any]] = ???
}
