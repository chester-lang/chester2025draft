package chester.readerv2

import chester.error.{Pos, RangeInFile, Reporter, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import chester.utils.getCodePoints
import chester.syntax.IdentifierRules.{isIdentifierFirst, isIdentifierPart, isOperatorSymbol}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import scala.util.boundary
import scala.util.boundary.break
import scala.util.Try

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
  
  // Cache for UTF-16 positions to avoid expensive recalculations
  private val utf16PosCache = new java.util.HashMap[Int, Int]()
  
  // Initialize cache with position 0
  utf16PosCache.put(0, 0)
  
  // Get UTF-16 position from cache or calculate and cache it
  private def getUtf16Position(bytePos: Int): Int = {
    if (utf16PosCache.containsKey(bytePos)) {
      utf16PosCache.get(bytePos)
    } else {
      // If we have a nearby cached position, start from there to avoid recalculating from the beginning
      val (nearestPos, nearestUtf16) = findNearestCachedPosition(bytePos)
      
      // Calculate the UTF-16 position incrementally from the nearest cached position
      val utf16Pos = if (nearestPos == bytePos) {
        nearestUtf16
      } else {
        nearestUtf16 + source.substring(nearestPos, bytePos).getCodePoints.size
      }
      
      // Cache the result
      utf16PosCache.put(bytePos, utf16Pos)
      utf16Pos
    }
  }
  
  // Find the nearest cached position that's less than or equal to the target position
  private def findNearestCachedPosition(targetPos: Int): (Int, Int) = {
    var nearestPos = 0
    var nearestUtf16 = 0
    
    val iter = utf16PosCache.entrySet().iterator()
    while (iter.hasNext()) {
      val entry = iter.next()
      val pos = entry.getKey()
      if (pos <= targetPos && pos > nearestPos) {
        nearestPos = pos
        nearestUtf16 = entry.getValue()
      }
    }
    
    (nearestPos, nearestUtf16)
  }
  
  // Update position with proper cache handling
  private def updatePosition(newPos: Int): Unit = {
    // Only update cache if moving forward (which is the common case)
    if (newPos > pos) {
      // Calculate and cache UTF-16 position for the new position
      // We can calculate incrementally from the current position
      if (!utf16PosCache.containsKey(newPos) && utf16PosCache.containsKey(pos)) {
        val currentUtf16 = utf16PosCache.get(pos)
        val incrementalUtf16 = currentUtf16 + source.substring(pos, newPos).getCodePoints.size
        utf16PosCache.put(newPos, incrementalUtf16)
      }
    }
    
    // Update the position
    pos = newPos
  }

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
    
    // Use cached UTF-16 positions or calculate them
    val startUtf16: Int :| Positive0 = Math
      .max(
        startPos,
        getUtf16Position(startPos)
      )
      .refineUnsafe[Positive0]
      
    val endUtf16: Int :| Positive0 = Math
      .max(
        endPos,
        getUtf16Position(endPos)
      )
      .refineUnsafe[Positive0]
      
    val lineUtf16: Int :| Positive0 = line.refineUnsafe[Positive0]
    val colUtf16: Int :| Positive0 = col.refineUnsafe[Positive0]

    val startPosition = Pos(WithUTF16(start, startUtf16), lineUtf16, WithUTF16(zero, colUtf16))
    val endPosition = Pos(WithUTF16(end, endUtf16), lineUtf16, WithUTF16((end - start).refineUnsafe, (endUtf16 - startUtf16).refineUnsafe))

    SourcePos(sourceOffset, RangeInFile(startPosition, endPosition))
  }

  private def nextToken: Either[ParseError, Token] = {
    // Check for whitespace tokens first
    val whitespaceToken = skipWhitespace()
    if (whitespaceToken.isDefined) {
      return Right(whitespaceToken.get)
    }
    
    if (pos >= source.length) {
      Right(Token.EOF(createSourcePos(0, 0)))
    } else {
      val c = source.codePointAt(pos)
      val startPos = pos
      val charCount = Character.charCount(c)
      // Use updatePosition to properly update cache
      updatePosition(pos + charCount)
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
          case d if d.isDigit                 => parseNumber(startPos)
          case a if a.isLetter || a == '_'    => parseIdentifier(a.toString, startPos)
          case o if isOperatorSymbol(o.toInt) => parseOperator(o.toString, startPos)
          case other =>
            Left(ParseError(s"Unexpected character: $other", createSourcePos(startPos, pos).range.start))
        }
      }
    }
  }

  private def skipWhitespace(): Option[Token] = {
    val startPos = pos
    
    var foundNewline = false
    var newlineStartPos = -1
    
    while (pos < source.length && source(pos).isWhitespace) {
      if (source(pos) == '\n') {
        if (!foundNewline) {
          // Track the position of first newline
          foundNewline = true
          newlineStartPos = pos
        }
        line += 1
        col = 0
      } else {
        col += 1
      }
      pos += 1
    }
    
    // If we moved, update the cache for the new position
    if (pos > startPos) {
      // We've already updated pos directly, so we need to update the cache
      if (!utf16PosCache.containsKey(pos) && utf16PosCache.containsKey(startPos)) {
        val startUtf16 = utf16PosCache.get(startPos)
        val incrementalUtf16 = startUtf16 + source.substring(startPos, pos).getCodePoints.size
        utf16PosCache.put(pos, incrementalUtf16)
      }
    }
    
    // If we found a newline, create a Newline token
    if (foundNewline) {
      Some(Token.Newline(createSourcePos(newlineStartPos, newlineStartPos + 1)))
    } else if (pos > startPos) {
      // If we found other whitespace, create a Whitespace token
      Some(Token.Whitespace(createSourcePos(startPos, pos)))
    } else {
      None // No whitespace found
    }
  }

  // Function to handle escape sequences in strings
  private def parseEscapeSequence(startPos: Int): Either[ParseError, (String, Int)] = {
    if (startPos >= source.length) {
      return Left(ParseError("Unexpected end of input in escape sequence", createSourcePos(startPos - 1, startPos).range.start))
    }
    
    val c = source(startPos)
    c match {
      // Standard single-character escapes
      case 'n'  => Right(("\n", startPos + 1))
      case 't'  => Right(("\t", startPos + 1))
      case 'r'  => Right(("\r", startPos + 1))
      case '"'  => Right(("\"", startPos + 1))
      case '\\' => Right(("\\", startPos + 1))
      case 'b'  => Right(("\b", startPos + 1))
      case 'f'  => Right(("\f", startPos + 1))
      
      // Unicode escapes \uXXXX
      case 'u' => 
        if (startPos + 4 >= source.length) {
          Left(ParseError("Incomplete Unicode escape sequence", createSourcePos(startPos - 1, startPos + 1).range.start))
        } else {
          val hexDigits = source.substring(startPos + 1, startPos + 5)
          Try(Integer.parseInt(hexDigits, 16)).toEither match {
            case Right(codePoint) => Right((new String(Character.toChars(codePoint)), startPos + 5))
            case Left(_) => Left(ParseError(s"Invalid Unicode escape sequence \\u$hexDigits", createSourcePos(startPos - 1, startPos + 5).range.start))
          }
        }
      
      // Hex escapes \xXX
      case 'x' =>
        if (startPos + 2 >= source.length) {
          Left(ParseError("Incomplete hex escape sequence", createSourcePos(startPos - 1, startPos + 1).range.start))
        } else {
          val hexDigits = source.substring(startPos + 1, startPos + 3)
          Try(Integer.parseInt(hexDigits, 16)).toEither match {
            case Right(charValue) => Right((charValue.toChar.toString, startPos + 3))
            case Left(_) => Left(ParseError(s"Invalid hex escape sequence \\x$hexDigits", createSourcePos(startPos - 1, startPos + 3).range.start))
          }
        }
      
      // Octal escapes (e.g., \123)
      case c if c >= '0' && c <= '7' =>
        var endPos = startPos + 1
        while (endPos < source.length && endPos < startPos + 3 && 
               source(endPos) >= '0' && source(endPos) <= '7') {
          endPos += 1
        }
        val octalDigits = source.substring(startPos, endPos)
        Try(Integer.parseInt(octalDigits, 8)).toEither match {
          case Right(charValue) if charValue <= 0xFF => Right((charValue.toChar.toString, endPos))
          case Right(_) => Left(ParseError(s"Octal escape sequence \\$octalDigits out of range", createSourcePos(startPos - 1, endPos).range.start))
          case Left(_) => Left(ParseError(s"Invalid octal escape sequence \\$octalDigits", createSourcePos(startPos - 1, endPos).range.start))
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
      Right(Token.SymbolLiteral(sb.toString, createSourcePos(startPos, pos)))
    }
  }

  // Helper method to read digits for a specific base
  private def readDigits(pos: Int, base: Int): (String, Int) = {
    val isValidDigit = base match {
      case 2 => (c: Char) => c == '0' || c == '1'
      case 16 => (c: Char) => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f')
      case _ => (c: Char) => c.isDigit
    }
    
    var result = ""
    var currentPos = pos
    
    while (currentPos < source.length && isValidDigit(source(currentPos))) {
      result += source(currentPos)
      currentPos += 1
    }
    
    (result, currentPos)
  }

  // Parse a hexadecimal number (0x...)
  private def parseHexNumber(startPos: Int): Either[ParseError, Token] = {
    val prefix = source.substring(startPos, startPos + 2) // "0x"
    val (digits, finalPos) = readDigits(startPos + 2, 16)
    
    if (digits.isEmpty) {
      return Left(ParseError("Expected hex digits after '0x'", createSourcePos(startPos + 2, startPos + 2).range.start))
    }
    
    // Update tokenizer state
    pos = finalPos
    col += finalPos - startPos
    
    val fullNumber = prefix + digits
    Right(Token.IntegerLiteral(fullNumber, createSourcePos(startPos, finalPos)))
  }

  // Parse a binary number (0b...)
  private def parseBinaryNumber(startPos: Int): Either[ParseError, Token] = {
    val prefix = source.substring(startPos, startPos + 2) // "0b"
    val (digits, finalPos) = readDigits(startPos + 2, 2)
    
    if (digits.isEmpty) {
      return Left(ParseError("Expected binary digits after '0b'", createSourcePos(startPos + 2, startPos + 2).range.start))
    }
    
    // Update tokenizer state
    pos = finalPos
    col += finalPos - startPos
    
    val fullNumber = prefix + digits
    Right(Token.IntegerLiteral(fullNumber, createSourcePos(startPos, finalPos)))
  }

  // Parse the exponent part of a number (e.g., e+10, E-5)
  private def parseExponent(startPos: Int, numBuilder: StringBuilder): (StringBuilder, Int) = {
    var endPos = startPos
    
    // Add 'e' or 'E'
    numBuilder.append(source(endPos))
    endPos += 1
    
    // Handle optional sign in exponent
    if (endPos < source.length && (source(endPos) == '+' || source(endPos) == '-')) {
      numBuilder.append(source(endPos))
      endPos += 1
    }
    
    // Read exponent digits
    val (expDigits, afterExp) = readDigits(endPos, 10)
    if (expDigits.isEmpty) {
      // We'll return the current state and let the caller handle the error
      return (numBuilder, endPos)
    }
    
    numBuilder.append(expDigits)
    (numBuilder, afterExp)
  }

  // Parse a decimal number (with optional decimal point and exponent)
  private def parseDecimalNumber(startPos: Int): Either[ParseError, Token] = {
    val (digits, finalPos) = readDigits(startPos, 10)
    
    if (digits.isEmpty) {
      return Left(ParseError("Expected digits", createSourcePos(startPos, startPos).range.start))
    }
    
    var endPos = finalPos
    val numBuilder = new StringBuilder(digits)
    
    // Check for decimal point
    if (endPos < source.length && source(endPos) == '.') {
      numBuilder.append('.')
      endPos += 1
      
      // Read decimal digits
      val (decimalDigits, afterDecimal) = readDigits(endPos, 10)
      numBuilder.append(decimalDigits)
      endPos = afterDecimal
    }
    
    // Check for exponent
    if (endPos < source.length && (source(endPos) == 'e' || source(endPos) == 'E')) {
      val (updatedBuilder, newEndPos) = parseExponent(endPos, numBuilder)
      
      // Check if exponent digits were found
      if (newEndPos == endPos + 1 || (newEndPos == endPos + 2 && (source(endPos + 1) == '+' || source(endPos + 1) == '-'))) {
        return Left(ParseError("Expected digits after exponent", createSourcePos(newEndPos, newEndPos).range.start))
      }
      
      endPos = newEndPos
    }
    
    // Update tokenizer state
    pos = endPos
    col += endPos - startPos
    
    // If we have a decimal point or exponent, it's a rational number
    if (numBuilder.toString.contains('.') || numBuilder.toString.toLowerCase.contains('e')) {
      Right(Token.RationalLiteral(numBuilder.toString, createSourcePos(startPos, endPos)))
    } else {
      Right(Token.IntegerLiteral(numBuilder.toString, createSourcePos(startPos, endPos)))
    }
  }

  private def parseNumber(startPos: Int): Either[ParseError, Token] = {
    // Check for hex or binary prefix
    if (startPos + 1 < source.length && source(startPos) == '0') {
      if (source(startPos + 1) == 'x') {
        return parseHexNumber(startPos)
      } else if (source(startPos + 1) == 'b') {
        return parseBinaryNumber(startPos)
      }
    }
    
    // Default to decimal number
    parseDecimalNumber(startPos)
  }

  // Helper method to check if a character at the current position is a valid identifier part
  private def isValidIdentifierPart(pos: Int): Boolean = {
    if (pos >= source.length) return false
    
    val c = source.codePointAt(pos)
    if (Character.isSupplementaryCodePoint(c)) {
      isIdentifierPart(c)
    } else {
      isIdentifierPart(c.toChar.toInt)
    }
  }

  private def parseIdentifier(initial: String, startPos: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < source.length && isValidIdentifierPart(pos)) {
      // Handle supplementary code points (like emoji) correctly
      val c = source.codePointAt(pos)
      val charCount = Character.charCount(c)
      
      sb.append(source.substring(pos, pos + charCount))
      pos += charCount
      col += 1  // Note: This might need adjustment for wide characters in the future
    }
    Right(Token.Identifier(Vector(StringChar(sb.toString, createSourcePos(startPos, pos))), createSourcePos(startPos, pos)))
  }

  // Separate method for parsing single-line comments
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
    Right(Token.Comment(commentText, createSourcePos(startPos, pos)))
  }

  private def parseOperator(initial: String, startPos: Int): Either[ParseError, Token] = {
    // Handle comments as a special case
    if (initial == "/" && pos < source.length && source(pos) == '/') {
      return parseComment(startPos)
    }
    
    val sb = new StringBuilder(initial)
    
    // Special case for the => operator
    if (initial == "=" && pos < source.length && source(pos) == '>') {
      pos += 1
      col += 1
      return Right(Token.Operator("=>", createSourcePos(startPos, pos)))
    }
    
    // Parse other operators
    while (pos < source.length && isOperatorSymbol(source(pos).toInt)) {
      sb.append(source(pos))
      pos += 1
      col += 1
    }
    
    val operator = sb.toString
    Right(Token.Operator(operator, createSourcePos(startPos, pos)))
  }

  // This method is now replaced by the more comprehensive parseEscapeSequence function
  private def escapeCharToString(c: Char): String = {
    c match {
      case 'n'  => "\n"
      case 't'  => "\t"
      case 'r'  => "\r"
      case '"'  => "\""
      case '\\' => "\\"
      case other => other.toString
    }
  }
}
