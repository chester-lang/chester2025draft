package chester.readerv2

import chester.error.{Pos, RangeInFile, Reporter, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import chester.syntax.IdentifierRules.{isIdentifierFirst, isIdentifierPart, isOperatorSymbol}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*

import scala.util.boundary
import scala.util.boundary.break
import scala.util.Try
import scala.collection.mutable

type TokenStream = LazyList[Either[ParseError, Token]]

object Tokenizer {
  def apply(sourceOffset: SourceOffset)(using Reporter[ParseError]): Tokenizer = new Tokenizer(sourceOffset)
  
  // Lookup tables for escape sequences
  private val simpleEscapes = Map(
    'n' -> "\n", 't' -> "\t", 'r' -> "\r", '"' -> "\"", 
    '\\' -> "\\", 'b' -> "\b", 'f' -> "\f"
  )
}

class Tokenizer(sourceOffset: SourceOffset)(using Reporter[ParseError]) {
  import Tokenizer._
  
  private val source = sourceOffset.readContent.getOrElse("")
  private var pos: Int = 0
  private var line: Int = 0
  private var col: Int = 0

  // Cache for UTF-16 positions
  private val utf16PosCache = collection.mutable.HashMap[Int, Int](0 -> 0)

  private def getUtf16Position(charPos: Int): Int = utf16PosCache.getOrElseUpdate(charPos, {
    val (nearestPos, nearestUtf16) = utf16PosCache.filter(_._1 <= charPos).maxBy(_._1)
    nearestUtf16 + source.substring(nearestPos, charPos).length()
  })

  def tokenize(): TokenStream = LazyList.unfold(false) { isEOF =>
    if (isEOF) None
    else if (pos >= source.length) Some((Right(Token.EOF(sourcePos(0, 0))), true))
    else Some((nextToken, false))
  }

  private def sourcePos(startPos: Int, endPos: Int): SourcePos = {
    val zero = 0.refineUnsafe[Positive0]
    val start = startPos.refineUnsafe[Positive0]
    val end = endPos.refineUnsafe[Positive0]
    val startUtf16 = getUtf16Position(startPos).refineUnsafe[Positive0]
    val endUtf16 = getUtf16Position(endPos).refineUnsafe[Positive0]
    val lineUtf16 = line.refineUnsafe[Positive0]
    val colUtf16 = col.refineUnsafe[Positive0]

    val startPosition = Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(zero, colUtf16))
    val endPosition = Pos(WithUTF16(end, endUtf16), lineUtf16, WithUTF16((end - start).refineUnsafe, (endUtf16 - startUtf16).refineUnsafe))

    SourcePos(sourceOffset, RangeInFile(startPosition, endPosition))
  }
  
  // Helper for token creation
  private def token[T <: Token](f: SourcePos => T, startPos: Int): Either[ParseError, T] = {
    val _ = getUtf16Position(pos)
    Right(f(sourcePos(startPos, pos)))
  }

  private def nextToken: Either[ParseError, Token] = {
    skipWhitespace()
    if (pos >= source.length) return Right(Token.EOF(sourcePos(0, 0)))

    val c = source.codePointAt(pos)
    val startPos = pos
    val charCount = Character.charCount(c)
    pos += charCount
    col += 1

    if (Character.isSupplementaryCodePoint(c)) {
      if (isIdentifierFirst(c)) parseIdentifier(String.valueOf(Character.toChars(c)), startPos)
      else Left(ParseError(s"Unexpected character: ${String.valueOf(Character.toChars(c))}", sourcePos(startPos, pos).range.start))
    } else {
      parseSimpleToken(c.toChar, startPos)
    }
  }
  
  private def parseSimpleToken(c: Char, startPos: Int): Either[ParseError, Token] = c match {
    case '('                            => token(Token.LParen(_), startPos)
    case ')'                            => token(Token.RParen(_), startPos)
    case '['                            => token(Token.LBracket(_), startPos)
    case ']'                            => token(Token.RBracket(_), startPos)
    case '{'                            => token(Token.LBrace(_), startPos)
    case '}'                            => token(Token.RBrace(_), startPos)
    case ','                            => token(Token.Comma(_), startPos)
    case ';'                            => token(Token.Semicolon(_), startPos)
    case ':'                            => token(Token.Colon(_), startPos)
    case '.'                            => token(Token.Dot(_), startPos)
    case '@'                            => token(Token.At(_), startPos)
    case '"'                            => parseString(startPos)
    case '\''                           => parseSymbol(startPos)
    case d if d.isDigit                 => parseNumber(startPos)
    case a if a.isLetter || a == '_'    => parseIdentifier(a.toString, startPos)
    case o if isOperatorSymbol(o.toInt) => parseOperator(o.toString, startPos)
    case other => Left(ParseError(s"Unexpected character: $other", sourcePos(startPos, pos).range.start))
  }

  private def skipWhitespace(): Unit = {
    val startPos = pos
    while (pos < source.length && source(pos).isWhitespace) {
      if (source(pos) == '\n') { line += 1; col = 0 } 
      else col += 1
      pos += 1
    }
    if (pos > startPos) {
      val _ = getUtf16Position(pos)
    }
  }

  private def parseEscapeSequence(startPos: Int): Either[ParseError, (String, Int)] = {
    if (startPos >= source.length) 
      return Left(ParseError("Unexpected end of input in escape sequence", sourcePos(startPos - 1, startPos).range.start))

    val c = source(startPos)
    c match {
      case c if simpleEscapes.contains(c) => Right((simpleEscapes(c), startPos + 1))
      
      case 'u' if startPos + 4 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 5)
        Try(Integer.parseInt(hexDigits, 16)).toEither match {
          case Right(cp) => Right((new String(Character.toChars(cp)), startPos + 5))
          case Left(_) => Left(ParseError(s"Invalid Unicode escape \\u$hexDigits", sourcePos(startPos - 1, startPos + 5).range.start))
        }
      case 'u' => Left(ParseError("Incomplete Unicode escape", sourcePos(startPos - 1, startPos + 1).range.start))

      case 'x' if startPos + 2 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 3)
        Try(Integer.parseInt(hexDigits, 16)).toEither match {
          case Right(v) => Right((v.toChar.toString, startPos + 3))
          case Left(_) => Left(ParseError(s"Invalid hex escape \\x$hexDigits", sourcePos(startPos - 1, startPos + 3).range.start))
        }
      case 'x' => Left(ParseError("Incomplete hex escape", sourcePos(startPos - 1, startPos + 1).range.start))

      case c if c >= '0' && c <= '7' =>
        val endPos = (startPos + 1 to Math.min(startPos + 3, source.length)).takeWhile(i => 
          i < source.length && source(i) >= '0' && source(i) <= '7'
        ).lastOption.getOrElse(startPos + 1)
        
        val octalDigits = source.substring(startPos, endPos)
        Try(Integer.parseInt(octalDigits, 8)).toEither match {
          case Right(v) if v <= 0xff => Right((v.toChar.toString, endPos))
          case Right(_) => Left(ParseError(s"Octal escape \\$octalDigits out of range", sourcePos(startPos - 1, endPos).range.start))
          case Left(_) => Left(ParseError(s"Invalid octal escape \\$octalDigits", sourcePos(startPos - 1, endPos).range.start))
        }

      case other => Right((other.toString, startPos + 1))
    }
  }

  private def parseString(startPos: Int): Either[ParseError, Token] = boundary {
    var currentPos = startPos + 1 // Skip initial quote
    var chars = Vector.empty[StringChar]
    var escaped = false

    while (currentPos < source.length) {
      val c = source(currentPos)
      if (escaped) {
        parseEscapeSequence(currentPos) match {
          case Right((text, newPos)) => {
            chars = chars :+ StringChar(text, sourcePos(currentPos - 1, newPos))
            escaped = false
            currentPos = newPos
          }
          case Left(error) => break(Left(error))
        }
      } else if (c == '\\') {
        escaped = true
        currentPos += 1
      } else if (c == '"') {
        pos = currentPos + 1
        col += currentPos - startPos + 1
        val _ = getUtf16Position(pos)
        break(Right(Token.StringLiteral(chars, sourcePos(startPos, currentPos + 1))))
      } else {
        chars = chars :+ StringChar(c.toString, sourcePos(currentPos, currentPos + 1))
        currentPos += 1
      }
    }
    Left(ParseError("Unterminated string literal", sourcePos(startPos, currentPos).range.start))
  }

  private def parseSymbol(startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder
    while (pos < source.length && (source(pos).isLetterOrDigit || source(pos) == '_')) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    if (sb.isEmpty) Left(ParseError("Empty symbol literal", sourcePos(startPos, pos).range.start))
    else token(Token.SymbolLiteral(sb.toString, _), startPos)
  }

  private def parseNumber(startPos: Int): Either[ParseError, Token] = {
    // Check for hex or binary
    if (startPos + 1 < source.length && source(startPos) == '0') {
      source(startPos + 1) match {
        case 'x' => readDigitSequence(startPos + 2, c => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f')) match {
          case (digits, endPos) if digits.nonEmpty => 
            pos = endPos; col += endPos - startPos
            return token(Token.IntegerLiteral(s"0x$digits", _), startPos)
          case _ => return Left(ParseError("Expected hex digits after '0x'", sourcePos(startPos + 2, startPos + 2).range.start))
        }
        case 'b' => readDigitSequence(startPos + 2, c => c == '0' || c == '1') match {
          case (digits, endPos) if digits.nonEmpty => 
            pos = endPos; col += endPos - startPos
            return token(Token.IntegerLiteral(s"0b$digits", _), startPos)
          case _ => return Left(ParseError("Expected binary digits after '0b'", sourcePos(startPos + 2, startPos + 2).range.start))
        }
        case _ => // Continue with decimal parsing
      }
    }

    // Handle decimal and floating point
    val (intPart, afterInt) = readDigitSequence(startPos, _.isDigit)
    if (intPart.isEmpty) return Left(ParseError("Expected digits", sourcePos(startPos, startPos).range.start))

    var endPos = afterInt
    val numBuilder = new StringBuilder(intPart)
    var isRational = false

    // Check for decimal point
    if (endPos < source.length && source(endPos) == '.') {
      isRational = true
      numBuilder.append('.')
      endPos += 1
      val (decimalDigits, afterDecimal) = readDigitSequence(endPos, _.isDigit)
      numBuilder.append(decimalDigits)
      endPos = afterDecimal
    }

    // Check for exponent
    if (endPos < source.length && (source(endPos) == 'e' || source(endPos) == 'E')) {
      isRational = true
      numBuilder.append(source(endPos))
      endPos += 1

      // Handle optional sign
      if (endPos < source.length && (source(endPos) == '+' || source(endPos) == '-')) {
        numBuilder.append(source(endPos))
        endPos += 1
      }

      val (expDigits, afterExp) = readDigitSequence(endPos, _.isDigit)
      if (expDigits.isEmpty) 
        return Left(ParseError("Expected digits after exponent", sourcePos(endPos, endPos).range.start))
      
      numBuilder.append(expDigits)
      endPos = afterExp
    }

    pos = endPos
    col += endPos - startPos
    
    val numStr = numBuilder.toString
    if (isRational) token(Token.RationalLiteral(numStr, _), startPos)
    else token(Token.IntegerLiteral(numStr, _), startPos)
  }
  
  private def readDigitSequence(startPos: Int, isValid: Char => Boolean): (String, Int) = {
    var currentPos = startPos
    val sb = new StringBuilder
    while (currentPos < source.length && isValid(source(currentPos))) {
      sb.append(source(currentPos))
      currentPos += 1
    }
    (sb.toString, currentPos)
  }

  private def parseIdentifier(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && isValidIdentifierPart(pos)) {
      val c = source.codePointAt(pos)
      val charCount = Character.charCount(c)
      sb.append(source.substring(pos, pos + charCount))
      pos += charCount
      col += 1
    }
    token(Token.Identifier(Vector(StringChar(sb.toString, sourcePos(startPos, pos))), _), startPos)
  }

  private def isValidIdentifierPart(pos: Int): Boolean = {
    if (pos >= source.length) return false
    val c = source.codePointAt(pos)
    if (Character.isSupplementaryCodePoint(c)) isIdentifierPart(c)
    else isIdentifierPart(c.toChar.toInt)
  }

  private def parseComment(startPos: Int): Either[ParseError, Token] = {
    pos += 1 // Skip the second '/'
    col += 1
    val commentStart = pos

    while (pos < source.length && source(pos) != '\n') {
      pos += 1
      col += 1
    }

    token(Token.Comment(source.substring(commentStart, pos), _), startPos)
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    // Handle comments
    if (initial == "/" && pos < source.length && source(pos) == '/') 
      return parseComment(startPos)

    val sb = new StringBuilder(initial)
    while (pos < source.length && isOperatorSymbol(source(pos).toInt)) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }

    token(Token.Operator(sb.toString, _), startPos)
  }
}
