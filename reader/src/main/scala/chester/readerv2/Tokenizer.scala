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
  def apply(sourceOffset: SourceOffset)(using Reporter[ParseError]): Tokenizer = {
    new Tokenizer(sourceOffset)
  }
}

class Tokenizer(sourceOffset: SourceOffset)(using Reporter[ParseError]) {
  private val source = sourceOffset.readContent.getOrElse("")
  private var pos: Int = 0
  private var line: Int = 0
  private var col: Int = 0

  // Cache for UTF-16 positions - maps character position to UTF-16 position
  // UTF-16 position is always >= character position
  private val utf16PosCache = collection.mutable.HashMap[Int, Int]()
  utf16PosCache(0) = 0 // Initialize cache with start position

  // Get UTF-16 position for a given character position
  private def getUtf16Position(charPos: Int): Int = {
    utf16PosCache.getOrElseUpdate(charPos, {
      // Find the nearest cached position less than or equal to the target
      val (nearestPos, nearestUtf16) = utf16PosCache
        .filter(_._1 <= charPos)
        .maxBy(_._1)
      
      // Calculate UTF-16 length by counting surrogate pairs properly
      val substring = source.substring(nearestPos, charPos)
      val utf16Count = substring.length()
      
      // Add the UTF-16 count to the nearest cached UTF-16 position
      nearestUtf16 + utf16Count
    })
  }

  def tokenize(): TokenStream = {
    LazyList.unfold(false) { isEOF =>
      if (isEOF) None
      else if (pos >= source.length) Some((Right(Token.EOF(createSourcePos(0, 0))), true))
      else Some((nextToken, false))
    }
  }

  private def createSourcePos(startPos: Int, endPos: Int): SourcePos = {
    val zero = 0.refineUnsafe[Positive0]
    val start: Int :| Positive0 = startPos.refineUnsafe[Positive0]
    val end: Int :| Positive0 = endPos.refineUnsafe[Positive0]

    // Use cached UTF-16 positions (never less than their char positions)
    val startUtf16: Int :| Positive0 = getUtf16Position(startPos).refineUnsafe[Positive0]
    val endUtf16: Int :| Positive0 = getUtf16Position(endPos).refineUnsafe[Positive0]

    val lineUtf16 = line.refineUnsafe[Positive0]
    val colUtf16 = col.refineUnsafe[Positive0]

    val startPosition = Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(zero, colUtf16))
    val endPosition = Pos(WithUTF16(end, endUtf16), lineUtf16, WithUTF16((end - start).refineUnsafe, (endUtf16 - startUtf16).refineUnsafe))

    SourcePos(sourceOffset, RangeInFile(startPosition, endPosition))
  }

  private def nextToken: Either[ParseError, Token] = {
    skipWhitespace()
    if (pos >= source.length) {
      return Right(Token.EOF(createSourcePos(0, 0)))
    }

    val c = source.codePointAt(pos)
    val startPos = pos
    val charCount = Character.charCount(c)
    pos += charCount
    col += 1

    if (Character.isSupplementaryCodePoint(c)) {
      if (isIdentifierFirst(c)) parseIdentifier(String.valueOf(Character.toChars(c)), startPos)
      else Left(ParseError(s"Unexpected character: ${String.valueOf(Character.toChars(c))}", createSourcePos(startPos, pos).range.start))
    } else {
      parseSimpleToken(c.toChar, startPos)
    }
  }
  
  // Consolidated token creation for simple token types
  private def parseSimpleToken(c: Char, startPos: Int): Either[ParseError, Token] = {
    c match {
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
      case d if d.isDigit                 => parseNumber(startPos)
      case a if a.isLetter || a == '_'    => parseIdentifier(a.toString, startPos)
      case o if isOperatorSymbol(o.toInt) => parseOperator(o.toString, startPos)
      case other                          => Left(ParseError(s"Unexpected character: $other", createSourcePos(startPos, pos).range.start))
    }
  }

  private def skipWhitespace(): Unit = {
    val startPos = pos
    while (pos < source.length && source(pos).isWhitespace) {
      if (source(pos) == '\n') {
        line += 1
        col = 0
      } else {
        col += 1
      }
      pos += 1
    }
    
    // Update cache if we moved
    if (pos > startPos) {
      val _ = getUtf16Position(pos)
    }
  }

  // Handle escape sequences in strings
  private def parseEscapeSequence(startPos: Int): Either[ParseError, (String, Int)] = {
    if (startPos >= source.length) {
      return Left(ParseError("Unexpected end of input in escape sequence", createSourcePos(startPos - 1, startPos).range.start))
    }

    val c = source(startPos)
    c match {
      // Standard single-character escapes
      case c if "ntr\"\\bf".contains(c) => 
        val escaped = c match {
          case 'n' => "\n"
          case 't' => "\t"
          case 'r' => "\r"
          case '"' => "\""
          case '\\' => "\\"
          case 'b' => "\b"
          case 'f' => "\f"
        }
        Right((escaped, startPos + 1))

      // Unicode escapes \uXXXX
      case 'u' if startPos + 4 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 5)
        Try(Integer.parseInt(hexDigits, 16)).toEither match {
          case Right(codePoint) => Right((new String(Character.toChars(codePoint)), startPos + 5))
          case Left(_) => Left(ParseError(s"Invalid Unicode escape sequence \\u$hexDigits", createSourcePos(startPos - 1, startPos + 5).range.start))
        }
      case 'u' => Left(ParseError("Incomplete Unicode escape sequence", createSourcePos(startPos - 1, startPos + 1).range.start))

      // Hex escapes \xXX
      case 'x' if startPos + 2 < source.length =>
        val hexDigits = source.substring(startPos + 1, startPos + 3)
        Try(Integer.parseInt(hexDigits, 16)).toEither match {
          case Right(charValue) => Right((charValue.toChar.toString, startPos + 3))
          case Left(_) => Left(ParseError(s"Invalid hex escape sequence \\x$hexDigits", createSourcePos(startPos - 1, startPos + 3).range.start))
        }
      case 'x' => Left(ParseError("Incomplete hex escape sequence", createSourcePos(startPos - 1, startPos + 1).range.start))

      // Octal escapes (e.g., \123)
      case c if c >= '0' && c <= '7' =>
        var endPos = startPos + 1
        while (endPos < source.length && endPos < startPos + 3 && source(endPos) >= '0' && source(endPos) <= '7') {
          endPos += 1
        }
        val octalDigits = source.substring(startPos, endPos)
        Try(Integer.parseInt(octalDigits, 8)).toEither match {
          case Right(charValue) if charValue <= 0xff => Right((charValue.toChar.toString, endPos))
          case Right(_) => Left(ParseError(s"Octal escape sequence \\$octalDigits out of range", createSourcePos(startPos - 1, endPos).range.start))
          case Left(_)  => Left(ParseError(s"Invalid octal escape sequence \\$octalDigits", createSourcePos(startPos - 1, endPos).range.start))
        }

      // Default - for unrecognized escapes, just use the character directly
      case other => Right((other.toString, startPos + 1))
    }
  }

  private def parseString(startPos: Int): Either[ParseError, Token] = {
    boundary[Either[ParseError, Token]] {
      var currentPos = startPos + 1 // Skip initial quote
      var chars = Vector.empty[StringChar]
      var escaped = false

      while (currentPos < source.length) {
        val c = source(currentPos)
        if (escaped) {
          parseEscapeSequence(currentPos) match {
            case Right((escapedText, newPos)) => {
              chars = chars :+ StringChar(escapedText, createSourcePos(currentPos - 1, newPos))
              escaped = false
              currentPos = newPos
            }
            case Left(error) => break(Left(error))
          }
        } else if (c == '\\') {
          escaped = true
          currentPos += 1
        } else if (c == '"') {
          pos = currentPos + 1 // Update position for tokenizer state
          col += currentPos - startPos + 1 // Update column
          // Cache the UTF-16 position for the end of the string
          val _ = getUtf16Position(pos)
          break(Right(Token.StringLiteral(chars, createSourcePos(startPos, currentPos + 1))))
        } else {
          chars = chars :+ StringChar(c.toString, createSourcePos(currentPos, currentPos + 1))
          currentPos += 1
        }
      }
      Left(ParseError("Unterminated string literal", createSourcePos(startPos, currentPos).range.start))
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
      // Cache the UTF-16 position
      val _ = getUtf16Position(pos)
      Right(Token.SymbolLiteral(sb.toString, createSourcePos(startPos, pos)))
    }
  }

  // Helper to read digits for a specific base
  private def readDigits(pos: Int, base: Int): (String, Int) = {
    val isValidDigit = base match {
      case 2  => (c: Char) => c == '0' || c == '1'
      case 16 => (c: Char) => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f')
      case _  => (c: Char) => c.isDigit
    }

    var currentPos = pos
    val sb = new StringBuilder
    while (currentPos < source.length && isValidDigit(source(currentPos))) {
      sb.append(source(currentPos))
      currentPos += 1
    }
    (sb.toString, currentPos)
  }

  private def parseNumber(startPos: Int): Either[ParseError, Token] = {
    // Check for hex or binary prefix
    if (startPos + 1 < source.length && source(startPos) == '0') {
      source(startPos + 1) match {
        case 'x' => return parseBaseNumber(startPos, 16, "0x")
        case 'b' => return parseBaseNumber(startPos, 2, "0b")
        case _ => // Continue with decimal parsing
      }
    }

    // Handle decimal numbers
    val (intPart, afterInt) = readDigits(startPos, 10)
    if (intPart.isEmpty) {
      return Left(ParseError("Expected digits", createSourcePos(startPos, startPos).range.start))
    }

    var endPos = afterInt
    val numBuilder = new StringBuilder(intPart)
    var isRational = false

    // Check for decimal point
    if (endPos < source.length && source(endPos) == '.') {
      isRational = true
      numBuilder.append('.')
      endPos += 1
      val (decimalDigits, afterDecimal) = readDigits(endPos, 10)
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

      val (expDigits, afterExp) = readDigits(endPos, 10)
      if (expDigits.isEmpty) {
        return Left(ParseError("Expected digits after exponent", createSourcePos(endPos, endPos).range.start))
      }
      numBuilder.append(expDigits)
      endPos = afterExp
    }

    // Update tokenizer state
    pos = endPos
    col += endPos - startPos
    
    // Cache the UTF-16 position
    val _ = getUtf16Position(pos)

    val numStr = numBuilder.toString
    if (isRational) {
      Right(Token.RationalLiteral(numStr, createSourcePos(startPos, endPos)))
    } else {
      Right(Token.IntegerLiteral(numStr, createSourcePos(startPos, endPos)))
    }
  }
  
  // Helper method to parse numbers with specific base (hex, binary)
  private def parseBaseNumber(startPos: Int, base: Int, prefix: String): Either[ParseError, Token] = {
    val (digits, finalPos) = readDigits(startPos + 2, base)
    if (digits.isEmpty) {
      return Left(ParseError(s"Expected ${base match {
        case 16 => "hex"
        case 2 => "binary"
        case _ => ""
      }} digits after '$prefix'", createSourcePos(startPos + 2, startPos + 2).range.start))
    }
    pos = finalPos
    col += finalPos - startPos
    
    // Cache the UTF-16 position
    val _ = getUtf16Position(pos)
    
    Right(Token.IntegerLiteral(s"$prefix$digits", createSourcePos(startPos, finalPos)))
  }

  // Check if a character at position is a valid identifier part
  private def isValidIdentifierPart(pos: Int): Boolean = {
    if (pos >= source.length) return false
    val c = source.codePointAt(pos)
    if (Character.isSupplementaryCodePoint(c)) isIdentifierPart(c)
    else isIdentifierPart(c.toChar.toInt)
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
    
    // Cache the UTF-16 position
    val _ = getUtf16Position(pos)
    
    Right(Token.Identifier(Vector(StringChar(sb.toString, createSourcePos(startPos, pos))), createSourcePos(startPos, pos)))
  }

  private def parseComment(startPos: Int): Either[ParseError, Token] = {
    pos += 1 // Skip the second '/'
    col += 1
    val commentStart = pos

    // Read until end of line
    while (pos < source.length && source(pos) != '\n') {
      pos += 1
      col += 1
    }

    val commentText = source.substring(commentStart, pos)
    
    // Cache the UTF-16 position
    val _ = getUtf16Position(pos)
    
    Right(Token.Comment(commentText, createSourcePos(startPos, pos)))
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    // Handle comments
    if (initial == "/" && pos < source.length && source(pos) == '/') {
      return parseComment(startPos)
    }

    // Parse operators
    val sb = new StringBuilder(initial)
    while (pos < source.length && isOperatorSymbol(source(pos).toInt)) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }

    // Cache the UTF-16 position
    val _ = getUtf16Position(pos)

    Right(Token.Operator(sb.toString, createSourcePos(startPos, pos)))
  }
}
