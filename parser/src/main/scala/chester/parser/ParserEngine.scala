package chester.parser

import chester.error.*
import chester.syntax.concrete.*
import chester.utils.term.*
import chester.utils.{StringIndex, WithUTF16, platformUseCRLF}
import fastparse.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*

import scala.util.*

object ParserEngine {

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
      linesOffset.refineUnsafe,
      WithUTF16(posOffsetUnicode.refineUnsafe, posOffsetUTF16.refineUnsafe)
    )
  }

  def checkInputStatus(currentInput: String): InputStatus = {
    checkUnclosedPairs(currentInput)
  }

  private def checkUnclosedPairs(input: String): InputStatus = {
    val stack = scala.collection.mutable.Stack[Char]()
    for ((char, index) <- input.zipWithIndex) {
      char match {
        case '(' | '{' | '[' => stack.push(char)
        case ')' =>
          if (stack.isEmpty || stack.pop() != '(')
            return InputStatus.Error(
              s"Unmatched closing parenthesis at position $index"
            )
        case ']' =>
          if (stack.isEmpty || stack.pop() != '[')
            return InputStatus.Error(
              s"Unmatched closing bracket at position $index"
            )
        case '}' =>
          if (stack.isEmpty || stack.pop() != '{')
            return InputStatus.Error(
              s"Unmatched closing brace at position $index"
            )
        case _ => // Ignore other characters
      }
    }
    if (stack.nonEmpty) InputStatus.Incomplete else InputStatus.Complete
  }

  private def parseCompleteExpression(
      input: String,
      linesOffset: Int :| Positive0,
      posOffset: WithUTF16
  ): Either[ParseError, ParsedExpr] = {
    parse(
      input,
      p =>
        new ParserInternal(
          SourceOffset(
            FileNameAndContent("repl", input),
            linesOffset = linesOffset,
            posOffset = posOffset
          )
        )(using p).exprEntrance
    ) match {
      case Parsed.Success(expr, _) => Right(expr)
      case f: Parsed.Failure       => Left(ParseError(f.msg, Pos.Zero))
    }
  }
}
