package chester.resolve

import chester.reader.ParseError
import chester.syntax.concrete.Expr

type ExprsMatcher = (Seq[Expr]) => Boolean

case class ExprsParser[+A](parser: PartialFunction[Seq[Expr], A], prettyError: Option[(Seq[Expr]) => ParseError] = None) {
  def map[B](f: A => B): ExprsParser[B] = copy(parser = parser.andThen(f))
}
type ExprsParserAny = ExprsParser[Any]

object Exprs {
  def parse(exprs: Seq[Expr], parsers: Seq[ExprsParser[Any]]): Either[Option[ParseError], Seq[Any]] = ???
  def combine[A, B](a: ExprsParser[A], b: ExprsParser[B]): ExprsParser[(A, B)] = ???
  def combine[A, B, C](a: ExprsParser[A], b: ExprsParser[B], c: ExprsParser[C]): ExprsParser[(A, B, C)] = ???
}
