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

type TokenStream = LazyList[Either[ParseError, Token]]

object Tokenizer {
  def apply(sourceOffset: SourceOffset)(using Reporter[ParseError]): Tokenizer = new Tokenizer(sourceOffset)
  
  private val simpleEscapes = Map(
    'n' -> "\n", 't' -> "\t", 'r' -> "\r", '"' -> "\"", 
    '\\' -> "\\", 'b' -> "\b", 'f' -> "\f"
  )
  
  private val simpleTokens: Map[Char, (SourcePos => Token)] = Map(
    '(' -> Token.LParen, ')' -> Token.RParen,
    '[' -> Token.LBracket, ']' -> Token.RBracket, 
    '{' -> Token.LBrace, '}' -> Token.RBrace,
    ',' -> Token.Comma, ';' -> Token.Semicolon,
    ':' -> Token.Colon, '.' -> Token.Dot, '@' -> Token.At
  )
}

class Tokenizer(sourceOffset: SourceOffset)(using Reporter[ParseError]) {
  import Tokenizer._
  
  private val source = sourceOffset.readContent.getOrElse("")
  private var pos, line, col, utf16Pos = 0

  def tokenize(): TokenStream = 
    LazyList.unfold(false)(isEOF => 
      if (isEOF) None
      else if (pos >= source.length) Some((Right(Token.EOF(makePos(0, 0))), true))
      else Some((nextToken, false))
    )

  private def makePos(startPos: Int, endPos: Int): SourcePos = {
    val zero = 0.refineUnsafe[Positive0]
    val start = startPos.refineUnsafe[Positive0]
    val end = endPos.refineUnsafe[Positive0]
    
    val startUtf16 = source.substring(0, startPos).length.refineUnsafe[Positive0]
    val endUtf16 = source.substring(0, endPos).length.refineUnsafe[Positive0]
    val lineUtf16 = line.refineUnsafe[Positive0]
    val colUtf16 = col.refineUnsafe[Positive0]

    val startPosition = Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(zero, colUtf16))
    val endPosition = Pos(WithUTF16(end, endUtf16), lineUtf16, WithUTF16((end - start).refineUnsafe, (endUtf16 - startUtf16).refineUnsafe))

    SourcePos(sourceOffset, RangeInFile(startPosition, endPosition))
  }

  private def token[T <: Token](f: SourcePos => T, startPos: Int): Either[ParseError, T] = 
    Right(f(makePos(startPos, pos)))

  private def parseError(msg: String, pos: Int): Left[ParseError, Nothing] = 
    Left(ParseError(msg, makePos(pos, pos).range.start))

  private def nextToken: Either[ParseError, Token] = {
    skipWhitespace()
    if (pos >= source.length) return Right(Token.EOF(makePos(0, 0)))

    val c = source.codePointAt(pos)
    val startPos = pos
    val charCount = Character.charCount(c)
    pos += charCount
    col += 1
    utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)

    if (Character.isSupplementaryCodePoint(c)) {
      if (isIdentifierFirst(c)) parseIdentifier(String.valueOf(Character.toChars(c)), startPos)
      else parseError(s"Unexpected character: ${String.valueOf(Character.toChars(c))}", startPos)
    } else c.toChar match {
      case c if simpleTokens.contains(c) => token(simpleTokens(c), startPos)
      case '"' => parseString(startPos)
      case '\'' => parseSymbol(startPos)
      case d if d.isDigit => parseNumber(startPos)
      case a if a.isLetter || a == '_' => parseIdentifier(a.toString, startPos)
      case o if isOperatorSymbol(o.toInt) => parseOperator(o.toString, startPos)
      case other => parseError(s"Unexpected character: $other", startPos)
    }
  }

  private def skipWhitespace(): Unit = 
    while (pos < source.length && source(pos).isWhitespace) {
      if (source(pos) == '\n') { line += 1; col = 0 } else col += 1
      pos += 1
      utf16Pos += 1
    }

  private def parseEscapeSequence(startPos: Int): Either[ParseError, (String, Int)] = {
    if (startPos >= source.length) 
      return parseError("Unexpected end of input in escape sequence", startPos - 1)

    source(startPos) match {
      case c if simpleEscapes.contains(c) => Right((simpleEscapes(c), startPos + 1))
      
      case 'u' if startPos + 4 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 5)
        Try(Integer.parseInt(hexDigits, 16)).toEither match {
          case Right(cp) => Right((new String(Character.toChars(cp)), startPos + 5))
          case Left(_) => parseError(s"Invalid Unicode escape \\u$hexDigits", startPos - 1)
        }
      
      case 'x' if startPos + 2 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 3)
        Try(Integer.parseInt(hexDigits, 16)).toEither
          .map(v => (v.toChar.toString, startPos + 3))
          .left.map(_ => ParseError(s"Invalid hex escape \\x$hexDigits", makePos(startPos - 1, startPos + 3).range.start))
      
      case c if c >= '0' && c <= '7' =>
        val endPos = (startPos + 1 to Math.min(startPos + 3, source.length))
          .takeWhile(i => i < source.length && source(i) >= '0' && source(i) <= '7')
          .lastOption.getOrElse(startPos + 1)
        
        val octalDigits = source.substring(startPos, endPos)
        Try(Integer.parseInt(octalDigits, 8)).toEither match {
          case Right(v) if v <= 0xff => Right((v.toChar.toString, endPos))
          case _ => parseError(s"Invalid octal escape \\$octalDigits", startPos - 1)
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
          case Right((text, newPos)) => 
            chars :+= StringChar(text, makePos(currentPos - 1, newPos))
            escaped = false
            currentPos = newPos
          case Left(error) => break(Left(error))
        }
      } else c match {
        case '\\' => escaped = true; currentPos += 1
        case '"' => 
          pos = currentPos + 1
          col += currentPos - startPos + 1
          utf16Pos += (pos - startPos)
          break(Right(Token.StringLiteral(chars, makePos(startPos, currentPos + 1))))
        case _ =>
          chars :+= StringChar(c.toString, makePos(currentPos, currentPos + 1))
          currentPos += 1
      }
    }
    parseError("Unterminated string literal", startPos)
  }

  private def consumeWhile(pred: Char => Boolean, sb: StringBuilder = new StringBuilder): String = {
    while (pos < source.length && pred(source(pos))) {
      sb.append(source(pos))
      pos += 1
      col += 1
      utf16Pos += 1
    }
    sb.toString
  }

  private def parseSymbol(startPos: Int): Either[ParseError, Token] = {
    val symbol = consumeWhile(c => c.isLetterOrDigit || c == '_')
    if (symbol.isEmpty) parseError("Empty symbol literal", startPos)
    else token(Token.SymbolLiteral(symbol, _), startPos)
  }

  private def parseNumber(startPos: Int): Either[ParseError, Token] = {
    // Check for hex or binary
    if (startPos + 1 < source.length && source(startPos) == '0') {
      source(startPos + 1) match {
        case 'x' => 
          pos = startPos + 2
          val hex = consumeWhile(c => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f'))
          if (hex.isEmpty) return parseError("Expected hex digits after '0x'", startPos + 2)
          return token(Token.IntegerLiteral(s"0x$hex", _), startPos)
        case 'b' => 
          pos = startPos + 2
          val bin = consumeWhile(c => c == '0' || c == '1')
          if (bin.isEmpty) return parseError("Expected binary digits after '0b'", startPos + 2)
          return token(Token.IntegerLiteral(s"0b$bin", _), startPos)
        case _ => // Continue with decimal parsing
      }
    }

    // Handle regular numbers: we need to preserve the original character
    val sb = new StringBuilder()
    if (startPos < source.length) {
      sb.append(source(startPos))
      pos = startPos + 1
      col += 1
      utf16Pos += 1
    }
    
    // Append remaining digits
    sb.append(consumeWhile(_.isDigit))
    
    var isRational = false
    // Parse decimal point and fraction
    if (pos < source.length && source(pos) == '.') {
      isRational = true
      sb.append('.')
      pos += 1; col += 1; utf16Pos += 1
      sb.append(consumeWhile(_.isDigit))
    }

    // Parse exponent
    if (pos < source.length && (source(pos) == 'e' || source(pos) == 'E')) {
      isRational = true
      sb.append(source(pos))
      pos += 1; col += 1; utf16Pos += 1

      if (pos < source.length && (source(pos) == '+' || source(pos) == '-')) {
        sb.append(source(pos))
        pos += 1; col += 1; utf16Pos += 1
      }

      val expDigits = consumeWhile(_.isDigit)
      if (expDigits.isEmpty) return parseError("Expected digits after exponent", pos)
      sb.append(expDigits)
    }
    
    val numStr = sb.toString
    if (isRational) token(Token.RationalLiteral(numStr, _), startPos)
    else token(Token.IntegerLiteral(numStr, _), startPos)
  }

  private def parseIdentifier(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && isValidIdentifierPart(pos)) {
      val c = source.codePointAt(pos)
      val charCount = Character.charCount(c)
      sb.append(source.substring(pos, pos + charCount))
      pos += charCount
      col += 1
      utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)
    }
    token(Token.Identifier(Vector(StringChar(sb.toString, makePos(startPos, pos))), _), startPos)
  }

  private def isValidIdentifierPart(pos: Int): Boolean = {
    if (pos >= source.length) return false
    val c = source.codePointAt(pos)
    if (Character.isSupplementaryCodePoint(c)) isIdentifierPart(c)
    else isIdentifierPart(c.toChar.toInt)
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    // Handle comments
    if (initial == "/" && pos < source.length && source(pos) == '/') {
      pos += 1; col += 1; utf16Pos += 1 // Skip the second '/'
      val comment = consumeWhile(_ != '\n')
      return token(Token.Comment(comment, _), startPos)
    }

    val op = initial + consumeWhile(c => isOperatorSymbol(c.toInt))
    token(Token.Operator(op, _), startPos)
  }
}
