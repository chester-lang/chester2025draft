package chester.readerv2

import chester.error.{Pos, Reporter}
import chester.utils.WithUTF16
import chester.syntax.IdentifierRules._
import chester.reader.{ParseError, SourceOffset}
import _root_.io.github.iltotore.iron._
import _root_.io.github.iltotore.iron.constraint.all._

class Tokenizer(sourceOffset: SourceOffset)(using reporter: Reporter[ParseError]) {
  private val content = sourceOffset.readContent match {
    case Right(content) => content
    case Left(error) => 
      reporter(error)
      ""
  }
  
  private var index = 0
  private var line = sourceOffset.linesOffset
  private var column = sourceOffset.posOffset
  private var utf16Column = sourceOffset.posOffset.utf16
  private var hasError = false
  
  private def currentPos: Pos = Pos(
    sourceOffset.posOffset + WithUTF16(index.refineUnsafe, utf16Column.refineUnsafe),
    line,
    column
  )
  
  private def reportError(message: String): Unit = {
    hasError = true
    reporter(ParseError(message, currentPos))
  }
  
  private def currentChar: Option[Char] = 
    if (index >= content.length) None else Some(content(index))
    
  private def peek: Option[Char] = 
    if (index + 1 >= content.length) None else Some(content(index + 1))
    
  private def advance(): Unit = {
    if (index < content.length) {
      val c = content(index)
      if (c == '\n') {
        line = (line + 1).refineUnsafe[Positive0]
        column = WithUTF16.Zero
        utf16Column = 0
      } else {
        val charWidth = if (c.isHighSurrogate) 2 else 1
        column = WithUTF16(
          (column.i + 1).refineUnsafe,
          (column.utf16 + charWidth).refineUnsafe
        )
        utf16Column = (utf16Column+charWidth).refineUnsafe
      }
      index += 1
    }
  }

  private def singleCharToken(kind: TokenKind): Token = {
    val pos = currentPos
    advance()
    TokenWithPos(kind, pos)
  }

  private def scanWhitespace(): Token = {
    val startPos = currentPos
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    
    while (currentChar.exists(_.isWhitespace)) {
      chars += currentChar.get
      advance()
    }
    
    TokenWithPos(TokenKind.Whitespace(chars.toVector), startPos)
  }

  private def scanComment(): Token = {
    val startPos = currentPos
    val chars = new scala.collection.mutable.ArrayBuffer[Char]()
    
    // Skip the two forward slashes
    advance(); advance()
    
    while (currentChar.exists(_ != '\n')) {
      chars += currentChar.get
      advance()
    }
    
    TokenWithPos(TokenKind.Comment(chars.toVector), startPos)
  }

  private def scanIdentifier(): Token = {
    val startPos = currentPos
    val parts = new scala.collection.mutable.ArrayBuffer[TokenKind.NamePart]()
    var currentPart = new scala.collection.mutable.ArrayBuffer[Char]()
    
    def flushPart(isOperator: Boolean): Unit = {
      if (currentPart.nonEmpty) {
        parts += (if (isOperator) TokenKind.OperatorPart(currentPart.toVector) 
                 else TokenKind.IdentifierPart(currentPart.toVector))
        currentPart.clear()
      }
    }
    
    while (currentChar.exists(c => isIdentifierMiddle(c) || isOperatorSymbol(c))) {
      val c = currentChar.get
      if (isOperatorSymbol(c)) {
        flushPart(false)
        currentPart += c
        flushPart(true)
      } else {
        currentPart += c
      }
      advance()
    }
    flushPart(false)
    
    TokenWithPos(TokenKind.Identifier(parts.toVector), startPos)
  }

  private def scanNumber(): Token = {
    val startPos = currentPos
    val numBuilder = new StringBuilder()
    var isHex = false
    var isBinary = false
    
    // Check for hex/binary prefix
    if (currentChar == Some('0') && peek.exists(p => p == 'x' || p == 'b')) {
      numBuilder += '0'
      advance()
      val prefix = currentChar.get
      numBuilder += prefix
      isHex = prefix == 'x'
      isBinary = prefix == 'b'
      advance()
    }
    
    // Scan digits
    while (currentChar.exists(c => 
      if (isHex) c.isDigit || ('a' to 'f').contains(c.toLower)
      else if (isBinary) c == '0' || c == '1'
      else c.isDigit || c == '.' || c == 'e' || c == 'E' || c == '-'
    )) {
      numBuilder += currentChar.get
      advance()
    }
    
    val numStr = numBuilder.toString
    if (numStr.exists(c => c == '.' || c == 'e' || c == 'E')) {
      TokenWithPos(TokenKind.RationalLiteral(BigDecimal(numStr)), startPos)
    } else {
      val radix = if (isHex) 16 else if (isBinary) 2 else 10
      val value = if (isHex || isBinary) 
        BigInt(numStr.substring(2), radix)
      else 
        BigInt(numStr, radix)
      TokenWithPos(TokenKind.IntegerLiteral(value, radix), startPos)
    }
  }

  private def scanString(): Token = {
    val startPos = currentPos
    advance() // skip opening quote
    val segments = new scala.collection.mutable.ArrayBuffer[TokenKind.StringSegment]()
    var currentChars = new scala.collection.mutable.ArrayBuffer[Char]()
    
    def flushChars(): Unit = {
      if (currentChars.nonEmpty) {
        segments += TokenKind.StringChars(currentChars.toVector)
        currentChars.clear()
      }
    }
    
    while (currentChar.exists(_ != '"')) {
      currentChar match {
        case Some('\\') =>
          flushChars()
          advance()
          currentChar match {
            case Some(c @ ('n'|'r'|'t'|'\\'|'"'|'\'')) =>
              segments += TokenKind.StringEscape(c)
              advance()
            case Some(c) =>
              reportError(s"Invalid escape sequence: \\$c")
              advance()
            case None =>
              reportError("Unterminated string literal")
              return TokenWithPos(TokenKind.StringLiteral(segments.toVector), startPos)
          }
        case Some(c) =>
          currentChars += c
          advance()
        case None =>
          reportError("Unterminated string literal")
          return TokenWithPos(TokenKind.StringLiteral(segments.toVector), startPos)
      }
    }
    
    flushChars()
    advance() // skip closing quote
    TokenWithPos(TokenKind.StringLiteral(segments.toVector), startPos)
  }

  private def scanSymbol(): Token = {
    val startPos = currentPos
    advance() // skip opening quote
    val segments = new scala.collection.mutable.ArrayBuffer[TokenKind.StringSegment]()
    var currentChars = new scala.collection.mutable.ArrayBuffer[Char]()
    
    def flushChars(): Unit = {
      if (currentChars.nonEmpty) {
        segments += TokenKind.StringChars(currentChars.toVector)
        currentChars.clear()
      }
    }
    
    while (currentChar.exists(c => isIdentifierMiddle(c) || isOperatorSymbol(c))) {
      currentChar match {
        case Some('\\') =>
          flushChars()
          advance()
          currentChar match {
            case Some(c @ ('n'|'r'|'t'|'\\'|'"'|'\'')) =>
              segments += TokenKind.StringEscape(c)
              advance()
            case Some(c) =>
              reportError(s"Invalid escape sequence: \\$c")
              advance()
            case None =>
              reportError("Unterminated symbol literal")
              return TokenWithPos(TokenKind.SymbolLiteral(segments.toVector), startPos)
          }
        case Some(c) =>
          currentChars += c
          advance()
        case None =>
          reportError("Unterminated symbol literal")
          return TokenWithPos(TokenKind.SymbolLiteral(segments.toVector), startPos)
      }
    }
    
    flushChars()
    TokenWithPos(TokenKind.SymbolLiteral(segments.toVector), startPos)
  }

  def nextToken(): Token = {
    if (hasError) return TokenWithPos(TokenKind.EOF, currentPos)
    
    currentChar match {
      case None => TokenWithPos(TokenKind.EOF, currentPos)
      case Some(c) => c match {
        case c if c.isWhitespace => scanWhitespace()
        case '/' if peek == Some('/') => scanComment()
        case c if isIdentifierFirst(c) => scanIdentifier()
        case c if c.isDigit => scanNumber()
        case '"' => scanString()
        case '\'' => scanSymbol()
        case '(' => singleCharToken(TokenKind.LParen)
        case ')' => singleCharToken(TokenKind.RParen)
        case '{' => singleCharToken(TokenKind.LBrace)
        case '}' => singleCharToken(TokenKind.RBrace)
        case '[' => singleCharToken(TokenKind.LBracket)
        case ']' => singleCharToken(TokenKind.RBracket)
        case ',' => singleCharToken(TokenKind.Comma)
        case '.' => singleCharToken(TokenKind.Dot)
        case '=' => singleCharToken(TokenKind.Equal)
        case '-' if peek == Some('>') => 
          val startPos = currentPos
          advance(); advance()
          TokenWithPos(TokenKind.Arrow, startPos)
        case c => 
          reportError(s"Unexpected character: $c")
          advance()
          nextToken()
      }
    }
  }
}