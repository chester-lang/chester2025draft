package chester.readerv2

import chester.error.{Pos, Reporter}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*

import scala.language.implicitConversions

class Tokenizer(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]) {
  private val content: String = sourceOffset.readContent match {
    case Right(content) => content
    case Left(error) => 
      reporter.report(error)
      ""
  }

  case class TokenizerState(
      index: Int,
      current: Option[Char],
      utf16: Int
  )

  private def currentPos(state: TokenizerState): Pos = {
    val line = content.substring(0, state.index).count(_ == '\n') + 1
    val lastNewline = content.lastIndexOf('\n', state.index - 1)
    val col = if (lastNewline == -1) state.index + 1 else state.index - lastNewline
    Pos(WithUTF16(col, state.utf16), line - 1, WithUTF16(col, state.utf16))
  }

  private def peek(state: TokenizerState): Option[Char] =
    if (state.index + 1 >= content.length) None else Some(content(state.index + 1))

  implicit def refineUUU(x: Int): Int :| Positive0 = x.refineUnsafe[Positive0]

  private def advance(state: TokenizerState): TokenizerState = {
    val c = state.current.getOrElse(return state)
    val nextIndex = state.index + 1
    val nextUtf16 = state.utf16 + (if (c.isHighSurrogate) 2 else 1)
    
    if (nextIndex >= content.length) {
      TokenizerState(nextIndex, None, nextUtf16)
    } else {
      TokenizerState(nextIndex, Some(content(nextIndex)), nextUtf16)
    }
  }

  private def scanIdentifier(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    var parts = Vector.empty[Token.IdentifierPart]
    val startPos = currentPos(state)

    while (current.current.exists(c => c.isLetterOrDigit || c == '_' || c == '-')) {
      val text = current.current.get.toString
      parts = parts :+ Token.NormalPart(text.toVector)
      current = advance(current)
    }

    if (parts.isEmpty) {
      (current, Left(ParseError("Expected identifier", startPos)))
    } else {
      (current, Right(Token.Identifier(parts, startPos)))
    }
  }

  private def scanString(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    var parts = Vector.empty[Token.StringPart]
    var currentChars = Vector.empty[Char]
    val startPos = currentPos(state)
    current = advance(current) // Skip opening quote

    while (current.current.isDefined) {
      val c = current.current.get
      if (c == '"') {
        if (currentChars.nonEmpty) {
          parts = parts :+ Token.StringChars(currentChars)
        }
        return (advance(current), Right(Token.StringLiteral(parts, startPos)))
      } else if (c == '\\') {
        if (currentChars.nonEmpty) {
          parts = parts :+ Token.StringChars(currentChars)
          currentChars = Vector.empty
        }
        peek(current) match {
          case Some(next) =>
            val escaped = next match {
              case 'n' => '\n'
              case 't' => '\t'
              case 'r' => '\r'
              case '"' => '"'
              case '\\' => '\\'
              case _ => next
            }
            parts = parts :+ Token.StringEscape(escaped)
            current = advance(advance(current))
          case None =>
            return (current, Left(ParseError("Incomplete escape sequence", currentPos(current))))
        }
      } else if (c == '$' && peek(current).contains('{')) {
        if (currentChars.nonEmpty) {
          parts = parts :+ Token.StringChars(currentChars)
          currentChars = Vector.empty
        }
        
        current = advance(advance(current)) // Skip ${
        var braceCount = 1
        var interpolationTokens = Vector.empty[Token]
        
        while (current.current.isDefined && braceCount > 0) {
          val c = current.current.get
          if (c == '{') {
            braceCount += 1
          } else if (c == '}') {
            braceCount -= 1
            if (braceCount == 0) {
              parts = parts :+ Token.StringInterpolation(interpolationTokens)
              current = advance(current)
              currentChars = Vector.empty
            }
          }
          
          // Parse a token from the interpolation
          val (nextState, token) = parseToken(current)
          token match {
            case Right(t) =>
              interpolationTokens = interpolationTokens :+ t
              current = nextState
            case Left(error) =>
              return (current, Left(error))
          }
        }
        
        if (braceCount > 0) {
          return (current, Left(ParseError("Unclosed brace in string interpolation", startPos)))
        }
      } else {
        currentChars = currentChars :+ c
        current = advance(current)
      }
    }

    (current, Left(ParseError("Unterminated string literal", startPos)))
  }

  private def parseNumber(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    var chars = Vector.empty[Char]
    val startPos = currentPos(state)

    while (current.current.exists(_.isDigit)) {
      chars = chars :+ current.current.get
      current = advance(current)
    }

    if (chars.isEmpty) {
      (current, Left(ParseError("Expected number", startPos)))
    } else {
      try {
        val value = BigInt(chars.mkString)
        (current, Right(Token.IntegerLiteral(value, 10, startPos)))
      } catch {
        case _: NumberFormatException =>
          (current, Left(ParseError("Invalid integer", startPos)))
      }
    }
  }

  private def parseToken(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    while (current.current.exists(_.isWhitespace)) {
      current = advance(current)
    }

    current.current match {
      case None => (current, Right(Token.EOF(currentPos(current))))
      case Some(c) =>
        val pos = currentPos(current)
        c match {
          case '(' => (advance(current), Right(Token.LParen(pos)))
          case ')' => (advance(current), Right(Token.RParen(pos)))
          case '{' => (advance(current), Right(Token.LBrace(pos)))
          case '}' => (advance(current), Right(Token.RBrace(pos)))
          case '[' => (advance(current), Right(Token.LBracket(pos)))
          case ']' => (advance(current), Right(Token.RBracket(pos)))
          case '.' => (advance(current), Right(Token.Dot(pos)))
          case ',' => (advance(current), Right(Token.Comma(pos)))
          case ';' => (advance(current), Right(Token.Semicolon(pos)))
          case ':' => (advance(current), Right(Token.Colon(pos)))
          case '"' => scanString(current)
          case c if c.isLetter || c == '_' => scanIdentifier(current)
          case c if c.isDigit => parseNumber(current)
          case c => (advance(current), Right(Token.Operator(c.toString, pos)))
        }
    }
  }

  def tokenize: TokenStream = {
    var current = TokenizerState(0, if (content.isEmpty) None else Some(content(0)), 0)
    LazyList.unfold(current) { state =>
      if (state.current.isEmpty) None
      else {
        val (nextState, token) = parseToken(state)
        Some((token, nextState))
      }
    }
  }
}
