package chester.resolve

import chester.error.*
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
  def caseClause(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[DesaltCaseClause] =
    (id(Const.Case) ~! any ~ id(Const.Arrow2) ~ any ^^ { case _ ~ pattern ~ _ ~ expr => DesaltCaseClause(pattern, expr, meta = opseq.meta) }) |||
      reporter.report(ExpectCase(opseq))

  extension [T](p: Parser[T]) {
    def |||(other: => Unit): Parser[T] = new Parser[T] {
      def apply(in: Input): ParseResult[T] =
        p(in) match {
          case success: Success[T]  => success
          case nosuccess: NoSuccess => other; nosuccess
        }
    }
  }

  def declTele(opseq: OpSeq)(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[DefTelescope] = ???

  def lambda(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[FunctionExpr] = ???

  def defined(using reporter: Reporter[TyckProblem]): Parser[Defined] = ???

  // TODO: actually implement this
  def decorationsOpt(using reporter: Reporter[TyckProblem]): Parser[Vector[Expr]] = success(Vector.empty)

  def letStmt(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    decorationsOpt ~ id(Const.Let) ~! defined ~ opt(id(Const.`:`) ~> any) ~ opt(id(Const.`=`) ~> any) ^^ { case decorations ~ _ ~ defn ~ typ ~ expr =>
      LetDefStmt(LetDefType.Let, defn, ty = typ, body = expr, decorations = decorations, meta = opseq.meta)
    }
      ||| reporter.report(ExpectLetDef(opseq))

  def parsers(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Expr] = caseClause(opseq) | lambda(opseq) | letStmt(opseq)

  def desalt(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr = expr match {
    case OpSeq(Seq(x), meta) => desalt(x.updateMeta(_.orElse(meta)))
    case opseq @ OpSeq(xs, meta) =>
      parsers(opseq)(SeqReader(xs)) match {
        case Success(result, next) => desalt(OpSeq((result +: next.asInstanceOf[SeqReader[Expr]].seq).toVector, meta))
        case _: NoSuccess          => expr
      }
    case obj: ObjectExpr => ObjectDesalt.desugarObjectExpr(obj)
    case expr            => expr
  }
}
