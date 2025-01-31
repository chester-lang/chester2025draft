package chester.readerv2

import chester.error.{Pos, Reporter}
import chester.reader.{ParseError, SourceOffset}
import chester.readerv2.Token.*
import chester.syntax.IdentifierRules.*
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

  private case class TokenizerState(
      index: Int,
      pos: Pos
  ) {
    def current: Option[Char] = if (index < content.length) Some(content(index)) else None
  }

  private def currentPos(state: TokenizerState): Pos = state.pos

  private def peek(state: TokenizerState): Option[Char] =
    if (state.index + 1 >= content.length) None else Some(content(state.index + 1))
  implicit def refineUUU(x:Int): Int :| Positive0 = x.refineUnsafe[Positive0]

  private def advance(state: TokenizerState): TokenizerState = {
    val c = state.current.getOrElse(return state)
    if (c == '\n') {
      state.copy(
        index = state.index + 1,
        pos = Pos(
          WithUTF16(state.pos.index.i + 1, state.pos.index.utf16 + 1),
          (state.pos.line + 1).refineUnsafe[Positive0],
          WithUTF16.Zero
        )
      )
    } else {
      val charWidth = if (c.isHighSurrogate) 2 else 1
      state.copy(
        index = state.index + 1,
        pos = Pos(
          WithUTF16(state.pos.index.i + charWidth, state.pos.index.utf16 + charWidth),
          state.pos.line,
          WithUTF16(state.pos.column.i + 1, state.pos.column.utf16 + charWidth)
        )
      )
    }
  }

  private def singleCharToken(state: TokenizerState, c: Char): (TokenizerState, Either[ParseError, Token]) = {
    val pos = currentPos(state)
    val nextState = advance(state)
    val token = c match {
      case '(' => LParen(pos)
      case ')' => RParen(pos)
      case '{' => LBrace(pos)
      case '}' => RBrace(pos)
      case '[' => LBracket(pos)
      case ']' => RBracket(pos)
      case ',' => Comma(pos)
      case '.' => Dot(pos)
      case '=' => Equal(pos)
      case ';' => Semicolon(pos)
      case '-' if peek(state) == Some('>') =>
        Arrow(pos)
      case c =>
        reporter(ParseError(s"Unexpected character: $c", pos))
        EOF(pos)
    }
    (nextState, Right(token))
  }

  private def scanWhitespace(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = state

    while (current.index < content.length && content(current.index).isWhitespace) {
      chars += content(current.index)
      current = advance(current)
    }

    (current, Right(Token.Whitespace(chars.toVector, startPos)))
  }

  private def scanComment(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = advance(advance(state)) // Skip //

    while (current.index < content.length && content(current.index) != '\n') {
      chars += content(current.index)
      current = advance(current)
    }

    (current, Right(SingleLineComment(chars.toVector, startPos)))
  }

  private def scanIdentifier(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    val startPos = currentPos(state)
    val parts = Vector.newBuilder[Token.IdentifierPart]
    var currentPart = Vector.newBuilder[Char]

    while (current.index < content.length && (isIdentifierPart(content(current.index)) || content(current.index) == '_')) {
      currentPart += content(current.index)
      current = advance(current)
    }

    if (currentPart.result().nonEmpty) {
      parts += Token.NormalPart(currentPart.result())
    }

    val identifier = Token.Identifier(parts.result(), startPos)
    (current, Right(identifier))
  }

  private def scanOperator(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    val startPos = currentPos(state)
    val parts = Vector.newBuilder[Token.IdentifierPart]
    var currentPart = Vector.newBuilder[Char]

    while (current.index < content.length && isOperatorChar(content(current.index))) {
      currentPart += content(current.index)
      current = advance(current)
    }

    if (currentPart.result().nonEmpty) {
      parts += Token.OpPart(currentPart.result())
    }

    val operator = Token.Operator(currentPart.result().mkString, startPos)
    (current, Right(operator))
  }

  private def parseNumber(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    val startPos = current.pos
    var chars = Vector.empty[Char]
    var isDecimal = false

    while (current.current.exists(c => c.isDigit || (!isDecimal && c == '.'))) {
      val c = current.current.get
      if (c == '.') {
        isDecimal = true
      }
      chars = chars :+ c
      current = advance(current)
    }

    if (isDecimal) {
      try {
        val value = chars.mkString.toDouble
        (current, Right(Token.RationalLiteral(value, startPos)))
      } catch {
        case _: NumberFormatException =>
          (current, Left(ParseError("Invalid decimal number", startPos)))
      }
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

  private def parseString(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = advance(state) // Skip opening quote
    val startPos = state.pos
    var parts = Vector.empty[StringPart]
    var currentChars = Vector.empty[Char]

    while (current.current.exists(_ != '"')) {
      val c = current.current.get
      if (c == '\\') {
        current = advance(current)
        if (current.current.isEmpty) {
          return (current, Left(ParseError("Unterminated escape sequence", startPos)))
        }
        val escaped = current.current.get match {
          case 'n' => '\n'
          case 't' => '\t'
          case 'r' => '\r'
          case '"' => '"'
          case '\\' => '\\'
          case c => return (current, Left(ParseError(s"Invalid escape sequence: \\$c", startPos)))
        }
        if (currentChars.nonEmpty) {
          parts = parts :+ StringChars(currentChars)
          currentChars = Vector.empty
        }
        parts = parts :+ StringEscape(escaped)
        current = advance(current)
      } else if (c == '$' && peek(current).contains('{')) {
        if (currentChars.nonEmpty) {
          parts = parts :+ StringChars(currentChars)
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
              parts = parts :+ StringInterpolation(interpolationTokens)
              current = advance(current)
              return (current, Right(Token.StringLiteral(parts, startPos)))
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
          return (current, Left(ParseError("Unterminated string interpolation", startPos)))
        }
      } else {
        currentChars = currentChars :+ c
        current = advance(current)
      }
    }

    if (currentChars.nonEmpty) {
      parts = parts :+ StringChars(currentChars)
    }

    if (current.current.isEmpty) {
      (current, Left(ParseError("Unterminated string literal", startPos)))
    } else {
      current = advance(current) // Skip closing quote
      (current, Right(Token.StringLiteral(parts, startPos)))
    }
  }

  private def isHexDigit(c: Char): Boolean =
    c.isDigit || ('a' to 'f').contains(c.toLower)

  private def isOperatorChar(c: Char): Boolean = {
    c match {
      case '+' | '-' | '*' | '/' | '%' | '=' | '<' | '>' | '!' | '&' | '|' | '^' | '~' | ':' => true
      case _ => false
    }
  }

  private def readOperator(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    var current = state
    val startPos = currentPos(state)
    val buffer = new StringBuilder()

    while (current.index < content.length && isOperatorChar(content(current.index))) {
      buffer.append(content(current.index))
      current = advance(current)
    }

    val op = buffer.toString()
    op match {
      case ":" => (current, Right(Token.Colon(startPos)))
      case "=" => (current, Right(Token.Equal(startPos)))
      case "->" => (current, Right(Token.Arrow(startPos)))
      case _ => (current, Right(Token.Operator(op, startPos)))
    }
  }

  private def parseToken(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val c = state.current.getOrElse(return (state, Right(Token.EOF(state.pos))))
    val (nextState, token) = c match {
      case c if c.isWhitespace => scanWhitespace(state)
      case '/' if peek(state).contains('/') => scanComment(state)
      case c if c.isDigit => parseNumber(state)
      case '"' => parseString(state)
      case c if isIdentifierFirst(c) => scanIdentifier(state)
      case c if isOperatorIdentifierFirst(c) => scanOperator(state)
      case c if isOperatorChar(c) => readOperator(state)
      case c => singleCharToken(state, c)
    }
    (nextState, token)
  }

  def tokenize: TokenStream = {
    var state = TokenizerState(
      0,
      Pos(WithUTF16(0, 0), 0, WithUTF16(0, 0))
    )
    var tokens = LazyList.empty[Either[ParseError, Token]]

    while (state.index < content.length) {
      val (newState, token) = parseToken(state)
      state = newState
      tokens = tokens.appended(token)
    }

    tokens.appended(Right(Token.EOF(currentPos(state))))
  }
}
