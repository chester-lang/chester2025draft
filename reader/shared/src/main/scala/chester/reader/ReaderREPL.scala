package chester.reader

import spire.math.Natural
import chester.syntax.concrete.*
import chester.utils.term.*
import chester.utils.{Nat, StringIndex, WithUTF16, platformUseCRLF}
import chester.i18n.*
import chester.readerv2.ChesterReaderV2

import scala.util.*
import scala.util.boundary
import scala.util.boundary.break

object ReaderREPL {

  def parseInput(
      history: Seq[String],
      currentInput: String,
      useCRLF: Boolean = platformUseCRLF
  ): Either[ParseError, ParsedExpr] = {
    // assert(history.last == currentInput) // doesn't hold for :t commands in repl
    val linesOffset = history.init.map(x => x.count(_ == '\n') + 1).sum
    val posOffsetUTF16 =
      history.init.map(x => x.length + (if (useCRLF) 2 else 1)).sum
    val posOffsetUnicode = history.init
      .map(x => StringIndex(x).unicodeLength + (if (useCRLF) 2 else 1))
      .sum

    parseCompleteExpression(
      currentInput,
      Nat(linesOffset),
      WithUTF16(Nat(posOffsetUnicode), Nat(posOffsetUTF16))
    )
  }

  def checkInputStatus(currentInput: String): InputStatus =
    checkUnclosedPairs(currentInput)

  private def checkUnclosedPairs(input: String): InputStatus = boundary {
    val stack = scala.collection.mutable.Stack[Char]()
    for ((char, index) <- input.zipWithIndex)
      char match {
        case '(' | '{' | '[' => stack.push(char)
        case ')' =>
          if (stack.isEmpty || stack.pop() != '(')
            break(
              InputStatus.Error(
                t"Unmatched closing parenthesis at position $index"
              )
            )
        case ']' =>
          if (stack.isEmpty || stack.pop() != '[')
            break(
              InputStatus.Error(
                t"Unmatched closing bracket at position $index"
              )
            )
        case '}' =>
          if (stack.isEmpty || stack.pop() != '{')
            break(
              InputStatus.Error(
                t"Unmatched closing brace at position $index"
              )
            )
        case _ => // Ignore other characters
      }
    if (stack.nonEmpty) InputStatus.Incomplete else InputStatus.Complete
  }

  private def parseCompleteExpression(
      input: String,
      linesOffset: Natural,
      posOffset: WithUTF16
  ): Either[ParseError, ParsedExpr] =
    ChesterReaderV2.parseExprWithOffset(
      sourceName = "repl",
      content = input,
      linesOffset = linesOffset,
      posOffset = posOffset
    )
}
