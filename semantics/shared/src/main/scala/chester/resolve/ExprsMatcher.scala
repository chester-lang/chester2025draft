package chester.resolve

import chester.reader.ParseError
import chester.syntax.concrete.Expr

type ExprsMatcher = (Seq[Expr]) => Boolean

trait ExprsParser[+A] {
  def parser: PartialFunction[Seq[Expr], A]
  def prettyError: Option[(Seq[Expr]) => ParseError] = None

  def map[B](f: A => B): ExprsParser[B] = new ExprsParser[B] {
    override def parser: PartialFunction[Seq[Expr], B] = ExprsParser.this.parser.andThen(f)
    override def prettyError: Option[(Seq[Expr]) => ParseError] = ExprsParser.this.prettyError
  }
}

trait Expr1Parser[+A] extends ExprsParser[A] {
  override final def parser: PartialFunction[Seq[Expr], A] = { case Seq(expr) => parse(expr) }
  def parse(expr: Expr): A

  override def map[B](f: A => B): ExprsParser[B] = new Expr1Parser[B] {
    override def parse(expr: Expr): B = f(Expr1Parser.this.parse(expr))
    override def prettyError: Option[(Seq[Expr]) => ParseError] = Expr1Parser.this.prettyError
  }
}

type ExprsParserAny = ExprsParser[Any]

object Exprs {
  def parse(exprs: Seq[Expr], parsers: Seq[ExprsParser[Any]]): Either[Option[ParseError], Seq[Any]] = ???
  def combine[A, B](a: ExprsParser[A], b: ExprsParser[B]): ExprsParser[(A, B)] = ???
  def combine[A, B, C](a: ExprsParser[A], b: ExprsParser[B], c: ExprsParser[C]): ExprsParser[(A, B, C)] = ???
}
