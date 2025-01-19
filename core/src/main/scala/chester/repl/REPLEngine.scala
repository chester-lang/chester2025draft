package chester.repl

import cats.implicits._
import chester.doc._
import chester.doc.const.{Colors, ReplaceBracketsWithWord}
import chester.error._
import chester.reader.{ParseError, ReaderREPL}
import chester.syntax.concrete.Expr
import chester.syntax.core._
import chester.tyck._
import chester.utils.doc._
import chester.utils.env
import chester.utils.env.Environment
import chester.utils.io._
import chester.utils.term._
import fansi._

// could be inline
def REPLEngine[F[_]](using
    runner: Runner[F],
    inTerminal: InTerminal[F],
    env: Environment
): F[Unit] = {
  given options: PrettierOptions = PrettierOptions.Default.updated(
    ReplaceBracketsWithWord,
    env.hasWindowsNarrator
  )

  val maxWidth = 80

  val mainPrompt: Str = Str("Chester> ").overlay(Colors.REPLPrompt.toFansi)
  val continuationPrompt0: Str =
    Str("...      ").overlay(Colors.REPLPrompt.toFansi)
  assert(mainPrompt.length == continuationPrompt0.length)

  val terminalInfo = new TerminalInfo {
    override def checkInputStatus(input: String): InputStatus =
      ReaderREPL.checkInputStatus(input)

    override def defaultPrompt: fansi.Str = mainPrompt

    override def continuationPrompt: fansi.Str = continuationPrompt0
  }

  // TODO: Add Out(n) to refer history evaluation

  // add to the environment of evaluation

  def startF: F[Unit] = {
    for {
      _ <- InTerminal.writeln("Welcome to the Chester REPL!")
      _ <- InTerminal.writeln(
        "Type your expressions below. Type 'exit' or ':q' to quit, ':?' for help."
      )
      _ <- InTerminal.writeln(
        s"OS: ${env.getOS} Arch: ${env.getArch} Environment: ${env.getRunningOn}"
      )
      _ <- runREPL
    } yield ()
  }

  def runREPL: F[Unit] = {
    InTerminal.readline(terminalInfo).flatMap {
      case LineRead(line) =>
        processLine(line).flatMap {
          case true  => runREPL
          case false => Runner.pure(())
        }
      case UserInterrupted =>
        for {
          _ <- InTerminal
            .writeln("User interrupted the input. Continuing the REPL.")
          _ <- runREPL
        } yield ()
      case EndOfFile =>
        InTerminal.writeln("End of input detected. Exiting REPL.")
      case StatusError(_) =>
        for {
          _ <- InTerminal.writeln("Error reading input. Continuing the REPL.")
          _ <- runREPL
        } yield ()
    }
  }

  def processLine(line: String): F[Boolean] = {
    line match {
      case "exit" | ":q" =>
        for {
          _ <-
            InTerminal.writeln("Exiting REPL.")
        } yield false
      case ":?" =>
        for {
          _ <- InTerminal.writeln("Commands:")
          _ <- InTerminal.writeln(":t <expr> - Check the type of an expression")
          _ <- InTerminal.writeln(":q - Quit the REPL")
          _ <- InTerminal.writeln(
            "You can also just type any expression to evaluate it."
          )
        } yield true
      case _ =>
        if (line.startsWith(":t ")) {
          val exprStr = line.drop(3)
          handleTypeCheck(exprStr).map(_ => true)
        } else {
          handleExpression(line).map(_ => true)
        }
    }
  }

  def handleTypeCheck(exprStr: String): F[Unit] =
    InTerminal.getHistory.flatMap { history =>
      ReaderREPL.parseInput(history, exprStr).fold(error => InTerminal.writeln(s"Parse Error: ${error.message}"), parsedExpr => typeCheck(parsedExpr) match {
            case TyckResult.Success(judge, _, _) =>
              InTerminal.writeln(prettyPrintJudge(judge))
            case TyckResult.Failure(errors, _, _, _) => printErrors(errors)
            case _                                   => unreachable()
          })
    }

  def handleExpression(line: String): F[Unit] = InTerminal.getHistory.flatMap { history =>
    ReaderREPL.parseInput(history, line).fold(error => InTerminal.writeln(s"Parse Error: ${error.message}"), parsedExpr => typeCheck(parsedExpr) match {
          case TyckResult.Success(judge, _, _) =>
            InTerminal.writeln(prettyPrintJudgeWellTyped(judge))
          case TyckResult.Failure(errors, _, _, _) => printErrors(errors)
          case _                                   => unreachable()
        })
  }

  def typeCheck(expr: Expr): TyckResult[?, Judge] = {
    Tycker.check(expr)
  }

  def printErrors(
      er: Vector[chester.error.TyckError],
      wr: Vector[chester.error.TyckWarning] = Vector()
  ): F[Unit] = {
    given sourceReader: SourceReader = SourceReader.default

    for {
      _ <- er.traverse(x => {
        InTerminal.writeln(
          FansiPrettyPrinter.render(x.renderDoc, maxWidth)
        )
      })
      _ <- wr.traverse(x => {
        InTerminal.writeln(
          FansiPrettyPrinter.render(x.renderDoc, maxWidth)
        )
      })
    } yield ()
  }

  def prettyPrintJudge(judge: Judge): String = {
    val termDoc = judge.wellTyped
    val typeDoc = judge.ty
    val effectDoc = judge.effects

    val doc = termDoc <+> Doc.text(
      ":"
    ) <+> typeDoc <+?> (judge.effects.nonEmpty, "/" <> effectDoc)

    FansiPrettyPrinter.render(doc, maxWidth).render
  }

  def prettyPrintJudgeWellTyped(judge: Judge): String = {
    val termDoc = judge.wellTyped
    FansiPrettyPrinter.render(termDoc, maxWidth).render
  }

  startF
}
