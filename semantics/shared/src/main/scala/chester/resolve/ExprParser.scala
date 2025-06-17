package chester.resolve

import chester.error.{Reporter, TyckProblem}
import chester.syntax.concrete.*
import chester.i18n.*
import chester.syntax.Const

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

case class SeqReader[+T](seq: Seq[T]) extends Reader[T] {
  override def first: T = seq.head
  override def atEnd: Boolean = seq.isEmpty
  override def rest: SeqReader[T] = SeqReader(seq.tail)
  override def drop(n: Int): SeqReader[T] = SeqReader(seq.drop(n))

  override def pos: Position = new Position {
    override def line: Int = 0

    override def column: Int = 0

    override protected def lineContents: String = ""
  }
}

object ExprParser extends Parsers {
  type Elem = Expr
  def any: Parser[Expr] =
    accept(t"any expression", { case e => e })
  def id(name: String): Parser[Expr] =
    accept(t"identifier $name", { case e: Identifier if e.name == name => e })
  def caseClause(meta: Option[ExprMeta])(using reporter: Reporter[TyckProblem]): Parser[DesaltCaseClause] =
    id(Const.Case) ~! any ~ id(Const.Arrow2) ~ any ^^ { case _ ~ pattern ~ _ ~ expr => DesaltCaseClause(pattern, expr, meta = meta) }

  def acceptOpSeq: Parser[OpSeq] = accept(t"operation sequence", { case ops: OpSeq => ops })
  def opseq[T](f: Option[ExprMeta] => Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(in: Input): ParseResult[T] = {
        val result = acceptOpSeq(in)
        result.flatMapWithNext { case OpSeq(ops, meta) => _ => f(meta)(SeqReader(ops)) }
      }
    }.named("opseq")

  def declTele(meta: Option[ExprMeta])(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[DefTelescope] = ???

  def lambda(meta: Option[ExprMeta])(using reporter: Reporter[TyckProblem]): Parser[FunctionExpr] = ???

  def letStmt(meta: Option[ExprMeta])(using reporter: Reporter[TyckProblem]): Parser[Stmt] = ???

  def parsers(meta: Option[ExprMeta])(using reporter: Reporter[TyckProblem]): Parser[Expr] = caseClause(meta) | lambda(meta) | letStmt(meta)

  def desalt(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr = expr match {
    case OpSeq(Seq(x), meta) => desalt(x.updateMeta(_.orElse(meta)))
    case OpSeq(xs, meta) =>
      parsers(meta)(SeqReader(xs)) match {
        case Success(result, next) => desalt(OpSeq((result +: next.asInstanceOf[SeqReader[Expr]].seq).toVector, meta))
        case _: NoSuccess          => expr
      }
    case obj: ObjectExpr => ObjectDesalt.desugarObjectExpr(obj)
    case expr            => expr
  }
}
