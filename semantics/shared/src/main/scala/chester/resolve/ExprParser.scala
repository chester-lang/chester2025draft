package chester.resolve

import chester.utils.elimNonEmptyVector
import chester.utils.{assumeNonEmpty, toNonEmptyVector}
import cats.data.{NonEmptySeq, NonEmptyVector}
import chester.error.*
import chester.syntax.concrete.*
import chester.i18n.*
import chester.syntax.Const
import chester.utils.reuse

import scala.annotation.tailrec
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
  val noInput = SeqReader(Seq.empty[Expr])
  type Elem = Expr
  def any: Parser[Expr] =
    accept(t"any expression", { case e => e })
  def id(name: String): Parser[Expr] =
    accept(t"identifier $name", { case e: Identifier if e.name == name => e })
  def anyid: Parser[Identifier] =
    accept(t"any identifier", { case e: Identifier => e })
  def anyopseq: Parser[OpSeq] =
    accept(t"any opseq", { case e: OpSeq => e })
  def anytuple: Parser[Tuple] =
    accept(t"any tuple", { case e: Tuple => e })
  def anylist: Parser[ListExpr] =
    accept(t"any list", { case e: ListExpr => e })
  def anyblock: Parser[Block] =
    accept(t"any block", { case e: Block => e })
  def anyFunctionCall: Parser[FunctionCall] =
    accept(t"any function call", { case e: FunctionCall => e })
  def caseClause(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[DesaltCaseClause] =
    id(Const.Case) ~! any ~ id(Const.Arrow2) ~ any ^^ { case _ ~ pattern ~ _ ~ expr => DesaltCaseClause(pattern, expr, meta = opseq.meta) }

  extension [T](p: Parser[T]) {
    def |-|(other: => Unit): Parser[T] = new Parser[T] {
      def apply(in: Input): ParseResult[T] =
        p(in) match {
          case success: Success[T]  => success
          case nosuccess: NoSuccess => other; nosuccess
        }
    }
  }

  def handleXs[A](xs: Seq[Expr], parser: Parser[A], fail: Input => ParseResult[A] = next => Failure("Expected end of input", next)): Parser[A] = in =>
    parser(SeqReader(xs)) match {
      case Success(result, next) =>
        if (next.atEnd) Success(result, in)
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

  def handleOneArgs(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[Seq[Arg]] = rep(handleOneArg)

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

  def declTele(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[DefTelescope] =
    handleOneArg ^^ { arg => DefTelescope(Vector(arg), meta = arg.meta) } | declTele1

  def declTele1(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[DefTelescope] =

    (anytuple flatMap { tuple =>
      handleXs(tuple.terms, handleOneArgs) ^^ { args => DefTelescope(args.toVector, implicitly = false, meta = tuple.meta) }
    }) | (anylist flatMap { tuple =>
      handleXs(tuple.terms, handleOneArgs) ^^ { args => DefTelescope(args.toVector, implicitly = true, meta = tuple.meta) }
    })

  def declTele1s(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Parser[NonEmptySeq[DefTelescope]] =
    rep1(declTele1) map (_.assumeNonEmpty)

  def lambda(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[FunctionExpr] = failure("TODO")

  // on DeclTeleMode.Default
  def processTelescope(x: MaybeTelescope)(using reporter: Reporter[TyckProblem]): DefTelescope = x match {
    case Tuple(xs, meta) =>
      val results = xs.map {
        case id: Identifier => Arg(name = Some(id), meta = id.meta)
        case opseq: OpSeq =>
          handleXs(opseq.seq, handleOneArgOpSeq(opseq))(noInput) match {
            case Success(arg, _) => arg
            case f: Failure      => ???
          }
      }
      DefTelescope(results, implicitly = false, meta = meta)
    case _ => ???
  }

  def defined(using reporter: Reporter[TyckProblem]): Parser[Defined] =
    anyid ~ declTele1s ^^ { case id ~ tele =>
      // TODO: correct meta
      DefinedFunction(id, tele.toNonEmptyVector, id.meta)
    } | anyid ^^ { id => DefinedPattern(PatternBind(id, id.meta), id.meta) } |
      anyFunctionCall ^^ { call =>
        call.function match {
          case id: Identifier => DefinedFunction(id, NonEmptyVector.of(processTelescope(call.telescope)), call.meta)
          case _              => ???
        }
      }

  // TODO: actually implement this
  def decorationsOpt(using reporter: Reporter[TyckProblem]): Parser[Vector[Expr]] = success(Vector.empty)

  @deprecated("this implementation is not correct")
  def combineMeta(metas: Seq[Option[ExprMeta]]): Option[ExprMeta] = None

  def buildOpseq(xs: Seq[Expr]): Expr = {
    require(xs.nonEmpty, "xs cannot be empty")
    if (xs.tail.isEmpty) return xs.head
    return OpSeq(xs.toVector, meta = combineMeta(xs.map(_.meta)))
  }

  def any1: Parser[Expr] = rep1(any) ^^ { xs => buildOpseq(xs) }

  def any1NoEq: Parser[Expr] = rep1(any - id(Const.`=`)) ^^ { xs => buildOpseq(xs) }

  def letStmt(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    decorationsOpt ~ id(Const.Let) ~! defined ~ opt(id(Const.`:`) ~> any1NoEq) ~ opt(id(Const.`=`) ~> any1) ^^ {
      case decorations ~ _ ~ defn ~ typ ~ expr =>
        LetDefStmt(LetDefType.Let, defn, ty = typ, body = expr, decorations = decorations, meta = opseq.meta)
    }

  def defStmt(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    decorationsOpt ~ id(Const.Def) ~! defined ~ opt(id(Const.`:`) ~> any1NoEq) ~ opt(id(Const.`=`) ~> any1) ^^ {
      case decorations ~ _ ~ defn ~ typ ~ expr =>
        LetDefStmt(LetDefType.Def, defn, ty = typ, body = expr, decorations = decorations, meta = opseq.meta)
    }

  def extensions(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Stmt] =
    id(Const.Extension) ~! declTele1s ~ anyblock ^^ { case _ ~ tele ~ body =>
      ExtensionStmt(tele.toNonEmptyVector, body, meta = opseq.meta)
    }

  def parsers(opseq: OpSeq)(using reporter: Reporter[TyckProblem]): Parser[Expr] =
    caseClause(opseq) | lambda(opseq) | letStmt(opseq) | defStmt(opseq) | extensions(opseq)

  @tailrec
  private def unwrap(e: Expr, next: Expr => Expr)(using Reporter[TyckProblem]): Expr =
    e match {
      case Block(Vector(), Some(tail), _) => unwrap(next(tail), next)
      case Tuple(Vector(term), _)         => unwrap(next(term), next)
      case e                              => e
    }

  def desaltUnwrap(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr =
    reuse(expr, unwrap(desalt(expr), desalt))

  def desugarTele(expr: Expr)(using mode: DeclTeleMode = DeclTeleMode.Default, reporter: Reporter[TyckProblem]): Option[DefTelescope] =
    declTele(SeqReader(Seq(expr))) match {
      case Success(telescope, next) =>
        Some(telescope)
      case failure: NoSuccess =>
        None
    }

  def desalt(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr = reuse(
    expr,
    expr match {
      case OpSeq(Vector(a, op: Identifier, b), meta) =>
        DotCall(a, op, Vector(DesaltCallingTelescope(Vector(CallingArg(expr = b, meta = b.meta)), meta = b.meta)), meta)

      case OpSeq(Seq(x), meta) => desalt(x.metaUpdated(_.orElse(meta)))
      case opseq @ OpSeq(xs, meta) =>
        parsers(opseq)(SeqReader(xs)) match {
          case Success(result, next) =>
            // add an assignment for debugging purposes
            val x = if (next.atEnd) result else expr
            x
          case _: NoSuccess =>
            // add an assignment for debugging purposes
            val x = expr
            x
        }
      case obj: ObjectExpr => ObjectDesalt.desugarObjectExpr(obj)
      case FunctionCall(function, telescopes, meta) =>
        val desugaredFunction = desalt(function)
        val desugaredTelescopes = telescopes match {
          case t: Tuple =>
            Vector(
              DesaltCallingTelescope(
                t.terms.map(term => CallingArg(expr = desalt(term), meta = term.meta)),
                meta = t.meta
              )
            )
          case other =>
            reporter.report(UnexpectedTelescope(other))
            Vector(
              DesaltCallingTelescope(
                Vector(CallingArg(expr = desalt(other), meta = other.meta)),
                meta = other.meta
              )
            )
        }
        desugaredFunction match {
          case DesaltFunctionCall(f, t, m) =>
            DesaltFunctionCall(f, t ++ desugaredTelescopes, m)
          case _ =>
            DesaltFunctionCall(
              desugaredFunction,
              desugaredTelescopes,
              meta
            )
        }
      case expr => expr
    }
  )
}
