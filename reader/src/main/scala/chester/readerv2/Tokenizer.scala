package chester.readerv2

import chester.error.{Pos, Reporter}
import chester.utils.WithUTF16
import chester.syntax.IdentifierRules.*
import chester.reader.{ParseError, SourceOffset}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.{Positive0 as IPositive0, *}
import chester.readerv2.Token.*

class Tokenizer(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]) {
  private val content = sourceOffset.readContent match {
    case Right(content) => content
    case Left(error) =>
      reporter(error)
      ""
  }

  private case class TokenizerState(
      index: Int,
      line: Int :| IPositive0,
      column: WithUTF16,
      utf16Column: Int :| IPositive0
  )

  private def currentPos(state: TokenizerState): Pos = Pos(
    sourceOffset.posOffset + WithUTF16(state.index.refineUnsafe, state.utf16Column),
    state.line,
    state.column
  )

  private def peek(state: TokenizerState): Option[Char] =
    if (state.index + 1 >= content.length) None else Some(content(state.index + 1))

  private def advance(state: TokenizerState): TokenizerState = {
    if (state.index < content.length) {
      val c = content(state.index)
      if (c == '\n') {
        state.copy(
          index = state.index + 1,
          line = (state.line + 1).refineUnsafe[IPositive0],
          column = WithUTF16.Zero,
          utf16Column = 0.refineUnsafe[IPositive0]
        )
      } else {
        val charWidth = if (c.isHighSurrogate) 2 else 1
        state.copy(
          index = state.index + 1,
          column = WithUTF16(
            (state.column.i + 1).refineUnsafe,
            (state.column.utf16 + charWidth).refineUnsafe
          ),
          utf16Column = (state.utf16Column + charWidth).refineUnsafe
        )
      }
    } else state
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
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = state

    while (current.index < content.length && isIdentifierPart(content(current.index))) {
      chars += content(current.index)
      current = advance(current)
    }

    (current, Right(Identifier(Vector(IdentifierPart(chars.toVector)), startPos)))
  }

  private def isHexDigit(c: Char): Boolean =
    c.isDigit || ('a' to 'f').contains(c.toLower)

  private def scanNumber(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = state
    var radix = 10

    if (content.startsWith("0x", current.index) || content.startsWith("0X", current.index)) {
      radix = 16
      chars += '0' += 'x'
      current = advance(advance(current))
    } else if (content.startsWith("0b", current.index) || content.startsWith("0B", current.index)) {
      radix = 2
      chars += '0' += 'b'
      current = advance(advance(current))
    }

    while (
      current.index < content.length &&
      (content(current.index).isDigit ||
        (radix == 16 && isHexDigit(content(current.index))) ||
        (radix == 2 && (content(current.index) == '0' || content(current.index) == '1')))
    ) {
      chars += content(current.index)
      current = advance(current)
    }

    if (current.index < content.length && content(current.index) == '.' && radix == 10) {
      chars += '.'
      current = advance(current)
      while (current.index < content.length && content(current.index).isDigit) {
        chars += content(current.index)
        current = advance(current)
      }
      val value = BigDecimal(chars.mkString)
      (current, Right(RationalLiteral(value, startPos)))
    } else {
      val value = radix match {
        case 16 => BigInt(chars.drop(2).mkString, 16)
        case 2 => BigInt(chars.drop(2).mkString, 2)
        case _ => BigInt(chars.mkString)
      }
      (current, Right(IntegerLiteral(value, radix, startPos)))
    }
  }

  private def scanString(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = advance(state) // Skip opening quote

    while (current.index < content.length && content(current.index) != '"') {
      if (content(current.index) == '\\' && current.index + 1 < content.length) {
        current = advance(current)
        chars += content(current.index)
      } else {
        chars += content(current.index)
      }
      current = advance(current)
    }

    if (current.index >= content.length) {
      (current, Left(ParseError("Unterminated string literal", startPos)))
    } else {
      current = advance(current) // Skip closing quote
      (current, Right(StringLiteral(Vector(StringChars(chars.toVector)), startPos)))
    }
  }

  private def scanSymbol(state: TokenizerState): (TokenizerState, Either[ParseError, Token]) = {
    val startPos = currentPos(state)
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    var current = advance(state) // Skip opening quote

    while (current.index < content.length && content(current.index) != '\'' && content(current.index) != '\n') {
      chars += content(current.index)
      current = advance(current)
    }

    if (current.index >= content.length || content(current.index) == '\n') {
      (current, Left(ParseError("Unterminated symbol literal", startPos)))
    } else {
      current = advance(current) // Skip closing quote
      (current, Right(SymbolLiteral(chars.mkString, startPos)))
    }
  }

  def tokenize: TokenStream = {
    def loop(state: TokenizerState): LazyList[Either[ParseError, Token]] = {
      if (state.index >= content.length) {
        LazyList(Right(EOF(currentPos(state))))
      } else {
        val c = content(state.index)
        val (nextState, result) = c match {
          case c if c.isWhitespace             => scanWhitespace(state)
          case '/' if peek(state) == Some('/') => scanComment(state)
          case c if isIdentifierFirst(c)       => scanIdentifier(state)
          case c if c.isDigit                  => scanNumber(state)
          case '"'                             => scanString(state)
          case '\''                            => scanSymbol(state)
          case c                               => singleCharToken(state, c)
        }
        result #:: loop(nextState)
      }
    }

    loop(TokenizerState(0, sourceOffset.linesOffset, sourceOffset.posOffset, sourceOffset.posOffset.utf16))
  }
}
