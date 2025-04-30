package chester.repl

import cats.implicits.*
import chester.doc.consts.{Colors, ReplaceBracketsWithWord}
import chester.error.*
import chester.reader.ReaderREPL
import chester.syntax.concrete.Expr
import chester.syntax.core.*
import chester.tyck.*
import chester.utils.doc.*
import chester.utils.env.Environment
import chester.utils.io.*
import chester.utils.term.*
import fansi.*
import chester.i18n.*

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

  // Track evaluation history
  var inputCounter = 1
  var evaluationHistory: List[Any] = List.empty
  var typeHistory: List[Term] = List.empty

  // Function to get previous evaluation result
  def out(n: Int): Any = {
    if (n <= 0 || n > evaluationHistory.length) {
      throw new IndexOutOfBoundsException(s"Out($n) is out of range. Available range: 1 to ${evaluationHistory.length}")
    }
    evaluationHistory(evaluationHistory.length - n)
  }

  // Create prompt with input counter
  def createMainPrompt(counter: Int): Str = {
    val promptText = s"In($counter) := "
    // Pad with spaces to ensure consistent length
    val paddedPrompt = promptText.padTo(10, ' ')
    Str(paddedPrompt).overlay(Colors.REPLPrompt.toFansi)
  }

  var mainPrompt: Str = createMainPrompt(inputCounter)
  val continuationPrompt0: Str =
    Str("... ").overlay(Colors.REPLPrompt.toFansi)
  // Ensure prompts have the same length // TODO: need to set continuationPrompt0 dynamically to match the length
  // assert(mainPrompt.length == continuationPrompt0.length)

  val terminalInfo = new TerminalInfo {
    override def checkInputStatus(input: String): InputStatus =
      ReaderREPL.checkInputStatus(input)

    override def defaultPrompt: fansi.Str = mainPrompt

    override def continuationPrompt: fansi.Str = continuationPrompt0
  }

  // Add Out(n) to refer history evaluation

  def startF: F[Unit] =
    for {
      _ <- InTerminal.writeln("Welcome to the Chester REPL!")
      _ <- InTerminal.writeln(
        "Type your expressions below. Type 'exit' or ':q' to quit, ':?' for help."
      )
      _ <- InTerminal.writeln(
        t"OS: ${env.getOS} Arch: ${env.getArch} Environment: ${env.getRunningOn}"
      )
      _ <- runREPL
    } yield ()

  def runREPL: F[Unit] =
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

  def processLine(line: String): F[Boolean] =
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
          _ <- InTerminal.writeln("Out(n) - Retrieve the nth previous evaluation result")
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

  def handleTypeCheck(exprStr: String): F[Unit] =
    InTerminal.getHistory.flatMap { history =>
      ReaderREPL
        .parseInput(history, exprStr)
        .fold(
          error => InTerminal.writeln(t"Parse Error: ${error.message}"),
          parsedExpr =>
            typeCheck(parsedExpr) match {
              case TyckResult.Success(judge, _, _) =>
                InTerminal.writeln(prettyPrintJudge(judge))
              case TyckResult.Failure(errors, _, _, _) => printErrors(errors)
              case _                                   => unreachable()
            }
        )
    }

  def handleExpression(line: String): F[Unit] = {
    // Special case for Out(n) function
    val outRegex = """Out\((\d+)\)""".r
    line match {
      case outRegex(nStr) =>
        try {
          val n = nStr.toInt
          if (n <= 0 || n > evaluationHistory.length) {
            InTerminal.writeln(t"Error: Out($n) is out of range. Available range: 1 to ${evaluationHistory.length}")
          } else {
            val result = out(n)
            InTerminal.writeln(t"${result}")
          }
        } catch {
          case e: NumberFormatException =>
            InTerminal.writeln(t"Error: Invalid number format in Out($nStr)")
        }
      case _ =>
        // Normal expression handling
        InTerminal.getHistory.flatMap { history =>
          ReaderREPL
            .parseInput(history, line)
            .fold(
              error => InTerminal.writeln(t"Parse Error: ${error.message}"),
              parsedExpr =>
                typeCheck(parsedExpr) match {
                  case TyckResult.Success(judge, _, _) =>
                    // Store the evaluation result and its type in history
                    evaluationHistory = evaluationHistory :+ judge.wellTyped
                    typeHistory = typeHistory :+ judge.ty

                    // Increment the input counter for the next prompt
                    inputCounter += 1
                    mainPrompt = createMainPrompt(inputCounter)

                    // Update the terminal info with the new prompt
                    terminalInfo match {
                      case info: TerminalInfo =>
                        // We can't modify the existing terminalInfo, but the next readline will use the updated mainPrompt
                    }

                    InTerminal.writeln(prettyPrintJudgeWellTyped(judge))
                  case TyckResult.Failure(errors, _, _, _) => printErrors(errors)
                  case _                                   => unreachable()
                }
            )
        }
    }
  }

  def typeCheck(expr: Expr): TyckResult[?, Judge] =
    Tycker.check(expr)

  def printErrors(
      er: Vector[chester.error.TyckError],
      wr: Vector[chester.error.TyckWarning] = Vector()
  ): F[Unit] = {
    given sourceReader: SourceReader = SourceReader.default

    for {
      _ <- er.traverse(x =>
        InTerminal.writeln(
          FansiPrettyPrinter.render(x.renderDoc, maxWidth)
        )
      )
      _ <- wr.traverse(x =>
        InTerminal.writeln(
          FansiPrettyPrinter.render(x.renderDoc, maxWidth)
        )
      )
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
