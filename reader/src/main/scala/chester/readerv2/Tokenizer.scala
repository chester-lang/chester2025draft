package chester.readerv2

import chester.error.{Pos, RangeInFile, Reporter, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import chester.utils.getCodePoints
import chester.syntax.IdentifierRules.{isIdentifierFirst, isOperatorSymbol}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import scala.util.boundary
import scala.util.boundary.break

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
        source.substring(0, startPos).getCodePoints.size
      )
      .refineUnsafe[Positive0]
    val endUtf16: Int :| Positive0 = Math
      .max(
        endPos,
        source.substring(0, endPos).getCodePoints.size
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
          case d if d.isDigit                 => parseNumber(startPos)
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
    boundary[Either[ParseError, Token]] {
      var currentPos = startPos + 1 // Skip initial quote
      var chars = Vector.empty[StringChar]
      var escaped = false

      while (currentPos < source.length) {
        val c = source(currentPos)
        if (escaped) {
          chars = chars :+ StringChar(escapeCharToString(c), createSourcePos(currentPos - 1, currentPos + 1))
          escaped = false
          currentPos += 1
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

  private def parseNumber(startPos: Int): Either[ParseError, Token] = {
    // Helper method to read digits for a specific base
    def readDigits(pos: Int, base: Int): (String, Int) = {
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

    val (prefix, digits, finalPos, base) = if (startPos + 1 < source.length) {
      if (source(startPos) == '0' && source(startPos + 1) == 'x') {
        val (hex, endPos) = readDigits(startPos + 2, 16)
        (source.substring(startPos, startPos + 2), hex, endPos, 16)
      } else if (source(startPos) == '0' && source(startPos + 1) == 'b') {
        val (bin, endPos) = readDigits(startPos + 2, 2)
        (source.substring(startPos, startPos + 2), bin, endPos, 2)
      } else {
        ("", readDigits(startPos, 10)._1, readDigits(startPos, 10)._2, 10)
      }
    } else {
      ("", readDigits(startPos, 10)._1, readDigits(startPos, 10)._2, 10)
    }

    if (digits.isEmpty && prefix.isEmpty) {
      return Left(ParseError("Expected digits", createSourcePos(startPos, startPos).range.start))
    }

    val isDecimal = prefix.isEmpty // Only decimal numbers can have decimal points or exponents
    
    // Check for decimal point or exponent
    if (isDecimal && finalPos < source.length && (source(finalPos) == '.' || source(finalPos) == 'e' || source(finalPos) == 'E')) {
      var endPos = finalPos
      val numBuilder = new StringBuilder(prefix + digits)
      
      // Handle decimal point
      if (finalPos < source.length && source(finalPos) == '.') {
        numBuilder.append('.')
        endPos += 1
        
        // Read decimal digits
        val (decimalDigits, afterDecimal) = readDigits(endPos, 10)
        numBuilder.append(decimalDigits)
        endPos = afterDecimal
      }
      
      // Handle exponent
      if (endPos < source.length && (source(endPos) == 'e' || source(endPos) == 'E')) {
        numBuilder.append(source(endPos)) // Add 'e' or 'E'
        endPos += 1
        
        // Handle optional sign in exponent
        if (endPos < source.length && (source(endPos) == '+' || source(endPos) == '-')) {
          numBuilder.append(source(endPos))
          endPos += 1
        }
        
        // Read exponent digits
        val (expDigits, afterExp) = readDigits(endPos, 10)
        if (expDigits.isEmpty) {
          return Left(ParseError("Expected digits after exponent", createSourcePos(endPos, endPos).range.start))
        }
        numBuilder.append(expDigits)
        endPos = afterExp
      }
      
      pos = endPos // Update position for tokenizer state
      col += endPos - startPos
      
      // Create token with the exact string needed for BigDecimal
      val finalNumber = numBuilder.toString()
      Right(Token.RationalLiteral(finalNumber, createSourcePos(startPos, endPos)))
    } else {
      // Simple integer
      pos = finalPos
      col += finalPos - startPos
      val fullNumber = prefix + digits
      Right(Token.IntegerLiteral(fullNumber, createSourcePos(startPos, finalPos)))
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
