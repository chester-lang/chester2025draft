package chester.readerv2

import chester.error.{Reporter, Pos}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*

type TokenStream = LazyList[Either[ParseError, Token]]

object Tokenizer {
  def apply(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]): Tokenizer = {
    new Tokenizer(sourceOffset)
  }
}

class Tokenizer(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]) {
  private val source = sourceOffset.source.readContent.getOrElse("")
  private var pos: Int = 0
  private var line: Int = 0
  private var col: Int = 0

  def tokenize(): TokenStream = {
    LazyList.unfold(()) { _ =>
      if (pos >= source.length) {
        Some((Right(Token.EOF(createPos(0))), ()))
      } else {
        val token = nextToken
        Some((token, ()))
      }
    }
  }

  private def createPos(startPos: Int): Pos = {
    val zero = 0.refineUnsafe[Positive0]
    val start = startPos.refineUnsafe[Positive0]
    val len = (pos - startPos).refineUnsafe[Positive0]
    val startUtf16 = source.substring(0, startPos).codePoints().count().toInt.refineUnsafe[Positive0]
    val lenUtf16 = source.substring(startPos, pos).codePoints().count().toInt.refineUnsafe[Positive0]
    val lineUtf16 = line.refineUnsafe[Positive0]
    val colUtf16 = col.refineUnsafe[Positive0]
    Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(len, lenUtf16))
  }

  private def nextToken: Either[ParseError, Token] = {
    skipWhitespace()
    if (pos >= source.length) {
      Right(Token.EOF(createPos(0)))
    } else {
      val c = source(pos)
      val startPos = pos
      pos += 1
      col += 1

      c match {
        case '(' => Right(Token.LParen(createPos(startPos)))
        case ')' => Right(Token.RParen(createPos(startPos)))
        case '[' => Right(Token.LBracket(createPos(startPos)))
        case ']' => Right(Token.RBracket(createPos(startPos)))
        case '{' => Right(Token.LBrace(createPos(startPos)))
        case '}' => Right(Token.RBrace(createPos(startPos)))
        case ',' => Right(Token.Comma(createPos(startPos)))
        case ';' => Right(Token.Semicolon(createPos(startPos)))
        case '=' => Right(Token.Equal(createPos(startPos)))
        case ':' => Right(Token.Colon(createPos(startPos)))
        case '.' => Right(Token.Dot(createPos(startPos)))
        case '@' => Right(Token.At(createPos(startPos)))
        case '"' => parseString(startPos)
        case '\'' => parseSymbol(startPos)
        case d if d.isDigit => parseNumber(d.toString, startPos)
        case a if a.isLetter || a == '_' => parseIdentifier(a.toString, startPos)
        case o if isOperatorChar(o) => parseOperator(o.toString, startPos)
        case other =>
          val error = ParseError(s"Unexpected character: $other", createPos(startPos))
          Left(error)
      }
    }
  }

  private def skipWhitespace(): Unit = {
    while (pos < source.length && source(pos).isWhitespace) {
      if (source(pos) == '\n') {
        line += 1
        col = 0
      } else {
        col += 1
      }
      pos += 1
    }
  }

  private def isOperatorChar(c: Char): Boolean = {
    c match {
      case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '~' | '<' | '>' | '!' | '?' => true
      case _ => false
    }
  }

  private def parseString(startPos: Int): Either[ParseError, Token] = {
    var chars = Vector.empty[StringChar]
    var escaped = false

    while (pos < source.length && (escaped || source(pos) != '"')) {
      if (escaped) {
        val escapeChar = source(pos) match {
          case 'n' => '\n'
          case 't' => '\t'
          case 'r' => '\r'
          case '"' => '"'
          case '\\' => '\\'
          case other =>
            val error = ParseError(s"Invalid escape sequence: \\$other", createPos(startPos))
            return Left(error)
        }
        chars = chars :+ StringChar(escapeChar.toString, createPos(pos - 1))
        escaped = false
      } else if (source(pos) == '\\') {
        escaped = true
      } else {
        chars = chars :+ StringChar(source(pos).toString, createPos(pos))
      }
      pos += 1
      col += 1
    }

    if (pos >= source.length) {
      val error = ParseError("Unterminated string literal", createPos(startPos))
      Left(error)
    } else {
      pos += 1 // Skip closing quote
      col += 1
      Right(Token.StringLiteral(chars, createPos(startPos)))
    }
  }

  private def parseSymbol(startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder
    while (pos < source.length && (source(pos).isLetterOrDigit || source(pos) == '_')) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    Right(Token.SymbolLiteral(sb.toString, createPos(startPos)))
  }

  private def parseNumber(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    var isRational = false

    while (pos < source.length && (source(pos).isDigit || source(pos) == '.')) {
      if (source(pos) == '.') {
        if (isRational) {
          val error = ParseError("Invalid number format: multiple decimal points", createPos(startPos))
          return Left(error)
        }
        isRational = true
      }
      sb.append(source(pos))
      pos += 1
      col += 1
    }

    if (isRational) {
      Right(Token.RationalLiteral(sb.toString, createPos(startPos)))
    } else {
      Right(Token.IntegerLiteral(sb.toString, createPos(startPos)))
    }
  }

  private def parseIdentifier(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && (source(pos).isLetterOrDigit || source(pos) == '_' || source(pos) == '-')) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    Right(Token.Identifier(Vector(StringChar(sb.toString, createPos(startPos))), createPos(startPos)))
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && isOperatorChar(source(pos))) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    Right(Token.Operator(sb.toString, createPos(startPos)))
  }
}
