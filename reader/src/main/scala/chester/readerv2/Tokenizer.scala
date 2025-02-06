package chester.readerv2

import chester.error.{Reporter, Pos, SourcePos, RangeInFile}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import chester.syntax.IdentifierRules.{isIdentifierFirst, isIdentifierPart, isOperatorSymbol}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*

type TokenStream = LazyList[Either[ParseError, Token]]

object Tokenizer {
  def apply(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]): Tokenizer = {
    new Tokenizer(sourceOffset)
  }
}

class Tokenizer(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]) {
  private val source = sourceOffset.readContent.getOrElse("")
  private var pos: Int = 0
  private var line: Int = 0
  private var col: Int = 0

  def tokenize(): TokenStream = {
    LazyList.unfold(false) { isEOF =>
      if (isEOF) {
        None // Stop generating tokens after EOF
      } else if (pos >= source.length) {
        Some((Right(Token.EOF(createSourcePos(0, 0))), true)) // Mark that we've hit EOF
      } else {
        val token = nextToken
        Some((token, false)) // Continue tokenizing
      }
    }
  }

  private def createSourcePos(startPos: Int, endPos: Int): SourcePos = {
    val zero = 0.refineUnsafe[Positive0]
    val start: Int :| Positive0 = startPos.refineUnsafe[Positive0]
    val end: Int :| Positive0 = endPos.refineUnsafe[Positive0]
    val startUtf16: Int :| Positive0 = Math
      .max(
        startPos,
        source.substring(0, startPos).codePoints().count().toInt
      )
      .refineUnsafe[Positive0]
    val endUtf16: Int :| Positive0 = Math
      .max(
        endPos,
        source.substring(0, endPos).codePoints().count().toInt
      )
      .refineUnsafe[Positive0]
    val lineUtf16: Int :| Positive0 = line.refineUnsafe[Positive0]
    val colUtf16: Int :| Positive0 = col.refineUnsafe[Positive0]

    val startPosition = Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(zero, colUtf16))
    val endPosition = Pos(WithUTF16(end, endUtf16), lineUtf16, WithUTF16((end - start).refineUnsafe, (endUtf16 - startUtf16).refineUnsafe))

    SourcePos(sourceOffset, RangeInFile(startPosition, endPosition))
  }

  private def nextToken: Either[ParseError, Token] = {
    skipWhitespace()
    if (pos >= source.length) {
      Right(Token.EOF(createSourcePos(0, 0)))
    } else {
      val c = source.codePointAt(pos)
      val startPos = pos
      pos += Character.charCount(c)
      col += 1

      if (Character.isSupplementaryCodePoint(c)) {
        if (isIdentifierFirst(c)) {
          parseIdentifier(String.valueOf(Character.toChars(c)), startPos)
        } else {
          Left(ParseError(s"Unexpected character: ${String.valueOf(Character.toChars(c))}", createSourcePos(startPos, pos).range.start))
        }
      } else {
        c.toChar match {
          case '('                            => Right(Token.LParen(createSourcePos(startPos, pos)))
          case ')'                            => Right(Token.RParen(createSourcePos(startPos, pos)))
          case '['                            => Right(Token.LBracket(createSourcePos(startPos, pos)))
          case ']'                            => Right(Token.RBracket(createSourcePos(startPos, pos)))
          case '{'                            => Right(Token.LBrace(createSourcePos(startPos, pos)))
          case '}'                            => Right(Token.RBrace(createSourcePos(startPos, pos)))
          case ','                            => Right(Token.Comma(createSourcePos(startPos, pos)))
          case ';'                            => Right(Token.Semicolon(createSourcePos(startPos, pos)))
          case ':'                            => Right(Token.Colon(createSourcePos(startPos, pos)))
          case '.'                            => Right(Token.Dot(createSourcePos(startPos, pos)))
          case '@'                            => Right(Token.At(createSourcePos(startPos, pos)))
          case '"'                            => parseString(startPos)
          case '\''                           => parseSymbol(startPos)
          case d if d.isDigit                 => parseNumber(d.toString, startPos)
          case a if a.isLetter || a == '_'    => parseIdentifier(a.toString, startPos)
          case o if isOperatorSymbol(o.toInt) => parseOperator(o.toString, startPos)
          case other =>
            Left(ParseError(s"Unexpected character: $other", createSourcePos(startPos, pos).range.start))
        }
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

  private def parseString(startPos: Int): Either[ParseError, Token] = {
    var chars = Vector.empty[StringChar]
    var escaped = false

    while (pos < source.length && (escaped || source(pos) != '"')) {
      if (escaped) {
        val escapeChar = source(pos) match {
          case 'n'  => '\n'
          case 't'  => '\t'
          case 'r'  => '\r'
          case '"'  => '"'
          case '\\' => '\\'
          case other =>
            val error = ParseError(s"Invalid escape sequence: \\$other", createSourcePos(pos - 1, pos).range.start)
            return Left(error)
        }
        chars = chars :+ StringChar(escapeChar.toString, createSourcePos(pos - 1, pos))
        escaped = false
      } else if (source(pos) == '\\') {
        escaped = true
      } else {
        chars = chars :+ StringChar(source(pos).toString, createSourcePos(pos, pos + 1))
      }
      pos += 1
      col += 1
    }

    if (pos >= source.length) {
      val error = ParseError("Unterminated string literal", createSourcePos(startPos, pos).range.start)
      Left(error)
    } else {
      pos += 1 // Skip closing quote
      col += 1
      Right(Token.StringLiteral(chars, createSourcePos(startPos, pos)))
    }
  }

  private def parseSymbol(startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder
    while (pos < source.length && (source(pos).isLetterOrDigit || source(pos) == '_')) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    if (sb.isEmpty) {
      Left(ParseError("Empty symbol literal", createSourcePos(startPos, pos).range.start))
    } else {
      Right(Token.SymbolLiteral(sb.toString, createSourcePos(startPos, pos)))
    }
  }

  private def parseNumber(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    var isRational = false
    var hasDecimalPoint = false
    var base = 10

    def readDigits(): Unit = {
      while (pos < source.length && source(pos).isDigit) {
        sb.append(source(pos))
        pos += 1
        col += 1
      }
    }

    def readHexDigits(): Unit = {
      while (pos < source.length && (source(pos).isDigit || ('a' <= source(pos).toLower && source(pos).toLower <= 'f'))) {
        sb.append(source(pos))
        pos += 1
        col += 1
      }
    }

    def readBinaryDigits(): Unit = {
      while (pos < source.length && (source(pos) == '0' || source(pos) == '1')) {
        sb.append(source(pos))
        pos += 1
        col += 1
      }
    }

    if (initial == "0" && pos < source.length) {
      source(pos) match {
        case 'x' | 'X' =>
          base = 16
          sb.append(source(pos)) // Keep the x
          pos += 1 // Skip 'x'
          col += 1
          readHexDigits()
        case 'b' | 'B' =>
          base = 2
          sb.append(source(pos)) // Keep the b
          pos += 1 // Skip 'b'
          col += 1
          readBinaryDigits()
        case _ =>
          readDigits()
      }
    } else {
      readDigits()
    }

    if (pos < source.length && source(pos) == '.') {
      if (base != 10) {
        return Left(ParseError("Decimal point not allowed in hex or binary numbers", createSourcePos(pos, pos + 1).range.start))
      }
      if (hasDecimalPoint) {
        return Left(ParseError("Multiple decimal points in number", createSourcePos(pos, pos + 1).range.start))
      }
      hasDecimalPoint = true
      isRational = true
      sb.append(source(pos))
      pos += 1
      col += 1
      readDigits()
    }

    // Handle exponent
    if (pos < source.length && (source(pos) == 'e' || source(pos) == 'E')) {
      if (base != 10) {
        return Left(ParseError("Exponent not allowed in hex or binary numbers", createSourcePos(pos, pos + 1).range.start))
      }
      isRational = true
      sb.append(source(pos))
      pos += 1
      col += 1

      // Optional sign
      if (pos < source.length && (source(pos) == '+' || source(pos) == '-')) {
        sb.append(source(pos))
        pos += 1
        col += 1
      }

      // Must have at least one digit after exponent
      if (pos >= source.length || !source(pos).isDigit) {
        return Left(ParseError("Expected digits after exponent", createSourcePos(pos, pos).range.start))
      }
      readDigits()
    }

    if (isRational) {
      Right(Token.RationalLiteral(sb.toString, createSourcePos(startPos, pos)))
    } else {
      Right(Token.IntegerLiteral(sb.toString, createSourcePos(startPos, pos)))
    }
  }

  private def parseIdentifier(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && (source(pos).isLetterOrDigit || source(pos) == '_' || source(pos) == '-')) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    Right(Token.Identifier(Vector(StringChar(sb.toString, createSourcePos(startPos, pos))), createSourcePos(startPos, pos)))
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    if (initial == "/" && pos < source.length && source(pos) == '/') {
      // Handle single-line comment
      pos += 1 // Skip the second '/'
      col += 1
      val commentStart = pos
      while (pos < source.length && source(pos) != '\n') {
        pos += 1
        col += 1
      }
      val commentText = source.substring(commentStart, pos)
      Right(Token.Comment(commentText, createSourcePos(startPos, pos)))
    } else {
      while (pos < source.length && isOperatorSymbol(source(pos).toInt)) {
        if (initial == "=" && source(pos) == '>') {
          pos += 1
          col += 1
          return Right(Token.Operator("=>", createSourcePos(startPos, pos)))
        }
        sb.append(source(pos))
        pos += 1
        col += 1
      }
      val operator = sb.toString
      Right(Token.Operator(operator, createSourcePos(startPos, pos)))
    }
  }
}
