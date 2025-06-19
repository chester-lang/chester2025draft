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
  def anyid: Parser[Identifier] =
    accept(t"any identifier", { case e: Identifier => e })
  def anyopseq: Parser[OpSeq] =
    accept(t"any opseq", { case e: OpSeq => e })
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

  def handleXs[A](xs: Seq[Expr], parser: Parser[A], fail: Input => ParseResult[A] = next => Failure("Expected end of input", next)): Parser[A] = _ =>
    parser(SeqReader(xs)) match {
      case Success(result, next) =>
        if (next.atEnd) Success(result, next)
        else fail(next)
      case failure: NoSuccess =>
        failure
    }

  def handleOneArg(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[Arg] = anyid ^^ { id =>
    mode match {
      case DeclTeleMode.Default =>
        Arg(name = Some(id), meta = id.meta)

      case DeclTeleMode.Type =>
        Arg(ty = Some(id), meta = id.meta)
    }
  } | (anyopseq flatMap { opseq => handleXs(opseq.seq, handleOneArgOpSeq(opseq)) })

  def handleOneArgOpSeq(opseq: OpSeq)(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[Arg] =
    anyid ~ opt(id(Const.`:`) ~> any) ~ opt(id(Const.`=`) ~> any) ^^ {
      case id ~ None ~ None =>
        mode match {
          case DeclTeleMode.Default =>
            Arg(name = Some(id), meta = opseq.meta)

          case DeclTeleMode.Type =>
            Arg(ty = Some(id), meta = opseq.meta)
        }
      case id ~ ty ~ expr =>
        Arg(name = Some(id), ty = ty, exprOrDefault = expr, meta = opseq.meta)
    }

  def declTele(opseq: OpSeq)(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[DefTelescope] = anyid ^^ {
    id =>
      mode match {
        case DeclTeleMode.Default =>

          DefTelescope(Vector(Arg(name = Some(id), meta = id.meta)), meta = opseq.meta)

        case DeclTeleMode.Type =>
          DefTelescope(Vector(Arg(ty = Some(id), meta = id.meta)), meta = opseq.meta)
      }
  } | (anyopseq flatMap { opseq => handleXs(opseq.seq, handleOneArgOpSeq(opseq)) } map { arg => DefTelescope(Vector(arg), meta = opseq.meta) }) |||
    reporter.report(???)

  def lambda(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[FunctionExpr] = ???

  def defined(using reporter: Reporter[TyckProblem]): Parser[Defined] = anyid ^^ { id => DefinedPattern(PatternBind(id, id.meta), id.meta) }

  // TODO: actually implement this
  def decorationsOpt(using reporter: Reporter[TyckProblem]): Parser[Vector[Expr]] = success(Vector.empty)

  def letStmt(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    decorationsOpt ~ id(Const.Let) ~! defined ~ opt(id(Const.`:`) ~> any) ~ opt(id(Const.`=`) ~> any) ^^ { case decorations ~ _ ~ defn ~ typ ~ expr =>
      LetDefStmt(LetDefType.Let, defn, ty = typ, body = expr, decorations = decorations, meta = opseq.meta)
    }
      ||| reporter.report(ExpectLetDef(opseq))

  def defStmt(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    decorationsOpt ~ id(Const.Def) ~! defined ~ opt(id(Const.`:`) ~> any) ~ opt(id(Const.`=`) ~> any) ^^ { case decorations ~ _ ~ defn ~ typ ~ expr =>
      LetDefStmt(LetDefType.Def, defn, ty = typ, body = expr, decorations = decorations, meta = opseq.meta)
    }
      ||| reporter.report(ExpectLetDef(opseq))
  def parsers(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Expr] = caseClause(opseq) | lambda(opseq) | letStmt(opseq) | defStmt(opseq)

  def desalt(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr = expr match {
    case OpSeq(Seq(x), meta) => desalt(x.updateMeta(_.orElse(meta)))
    case opseq @ OpSeq(xs, meta) =>
      parsers(opseq)(SeqReader(xs)) match {
        case Success(result, next) =>
          if (next.atEnd) result else expr
        case _: NoSuccess => expr
      }
    case obj: ObjectExpr => ObjectDesalt.desugarObjectExpr(obj)
    case expr            => expr
  }
}
