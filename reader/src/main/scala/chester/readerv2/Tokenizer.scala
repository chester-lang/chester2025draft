package chester.readerv2

import chester.error.{Pos, RangeInFile, Reporter, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.utils.WithUTF16
import chester.syntax.IdentifierRules.{isIdentifierFirst, isIdentifierPart, isOperatorSymbol}
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import scala.util.{Try, boundary}

type TokenStream = LazyList[Either[ParseError, Token]]

object Tokenizer {
  def apply(sourceOffset: SourceOffset)(using Reporter[ParseError]): Tokenizer = new Tokenizer(sourceOffset)
  private val escapes = Map('n' -> "\n", 't' -> "\t", 'r' -> "\r", '"' -> "\"", '\\' -> "\\", 'b' -> "\b", 'f' -> "\f")
  private val tokens: Map[Char, SourcePos => Token] = Map(
    '(' -> Token.LParen.apply, ')' -> Token.RParen.apply, 
    '[' -> Token.LBracket.apply, ']' -> Token.RBracket.apply, 
    '{' -> Token.LBrace.apply, '}' -> Token.RBrace.apply, 
    ',' -> Token.Comma.apply, ';' -> Token.Semicolon.apply,
    ':' -> Token.Colon.apply, '.' -> Token.Dot.apply, '@' -> Token.At.apply
  )
}

class Tokenizer(src: SourceOffset)(using Reporter[ParseError]) {
  import Tokenizer.*, boundary.break
  
  private val text = src.readContent.getOrElse("")
  private var pos, line, col, utf16Pos = 0

  def tokenize(): TokenStream = LazyList.unfold(false)(done => 
    if (done) None
    else if (pos >= text.length) Some((Right(Token.EOF(pos(0, 0))), true))
    else Some((nextToken, false))
  )

  private def pos(start: Int, end: Int) = {
    val zero = 0.refineUnsafe[Positive0]
    val lineRef = line.refineUnsafe[Positive0]
    val colRef = col.refineUnsafe[Positive0]
    val startRef = start.refineUnsafe[Positive0]
    val endRef = end.refineUnsafe[Positive0]
    val startU16 = text.substring(0, start).length.refineUnsafe[Positive0]
    val endU16 = text.substring(0, end).length.refineUnsafe[Positive0]
    
    SourcePos(src, RangeInFile(
      Pos(WithUTF16(startRef, startU16), lineRef, WithUTF16(zero, colRef)),
      Pos(WithUTF16(endRef, endU16), lineRef, WithUTF16((end - start).refineUnsafe, (endU16 - startU16).refineUnsafe))
    ))
  }

  private def err(msg: String, p: Int) = Left(ParseError(msg, pos(p, p).range.start))
  private def tok[T <: Token](f: SourcePos => T, start: Int) = Right(f(pos(start, pos)))

  private def nextToken: Either[ParseError, Token] = {
    while (pos < text.length && text(pos).isWhitespace) {
      if (text(pos) == '\n') { line += 1; col = 0 } else col += 1
      pos += 1; utf16Pos += 1
    }
    
    if (pos >= text.length) return Right(Token.EOF(pos(0, 0)))

    val c = text.codePointAt(pos)
    val start = pos
    pos += Character.charCount(c)
    col += 1
    utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)

    if (Character.isSupplementaryCodePoint(c)) {
      if (isIdentifierFirst(c)) parseIdent(String.valueOf(Character.toChars(c)), start)
      else err(s"Unexpected character: ${String.valueOf(Character.toChars(c))}", start)
    } else c.toChar match {
      case c if tokens.contains(c) => tok(tokens(c), start)
      case '"' => parseStr(start)
      case '\'' => parseSym(start)
      case d if d.isDigit => parseNum(start)
      case a if a.isLetter || a == '_' => parseIdent(a.toString, start)
      case o if isOperatorSymbol(o.toInt) => parseOp(o.toString, start)
      case x => err(s"Unexpected character: $x", start)
    }
  }
  
  private def read(pred: Char => Boolean, sb: StringBuilder = new StringBuilder): String = {
    while (pos < text.length && pred(text(pos))) {
      sb.append(text(pos))
      pos += 1; col += 1; utf16Pos += 1
    }
    sb.toString
  }

  private def parseEscape(start: Int): Either[ParseError, (String, Int)] = {
    if (start >= text.length) return err("Unexpected end of input in escape sequence", start - 1)

    text(start) match {
      case c if escapes.contains(c) => Right((escapes(c), start + 1))
      
      case 'u' if start + 4 < text.length =>
        val hex = text.substring(start + 1, start + 5)
        Try(Integer.parseInt(hex, 16)).toEither.map(cp => 
          (new String(Character.toChars(cp)), start + 5)
        ).left.map(_ => ParseError(s"Invalid Unicode escape \\u$hex", pos(start - 1, start + 5).range.start))
      
      case 'x' if start + 2 < text.length =>
        val hex = text.substring(start + 1, start + 3)
        Try(Integer.parseInt(hex, 16)).toEither.map(v => 
          (v.toChar.toString, start + 3)
        ).left.map(_ => ParseError(s"Invalid hex escape \\x$hex", pos(start - 1, start + 3).range.start))
      
      case c if c >= '0' && c <= '7' =>
        val end = (start + 1 to Math.min(start + 3, text.length))
          .takeWhile(i => i < text.length && text(i) >= '0' && text(i) <= '7')
          .lastOption.getOrElse(start + 1)
        
        val oct = text.substring(start, end)
        Try(Integer.parseInt(oct, 8)).toEither.map(v => 
          if (v <= 0xff) (v.toChar.toString, end) 
          else throw new NumberFormatException("Value outside byte range")
        ).left.map(_ => ParseError(s"Invalid octal escape \\$oct", pos(start - 1, end).range.start))

      case c => Right((c.toString, start + 1))
    }
  }

  private def parseStr(start: Int): Either[ParseError, Token] = boundary {
    var p = start + 1 // Skip initial quote
    var chars = Vector.empty[StringChar]
    var escaped = false

    while (p < text.length) {
      val c = text(p)
      if (escaped) {
        parseEscape(p) match {
          case Right((str, next)) => 
            chars :+= StringChar(str, pos(p - 1, next))
            escaped = false
            p = next
          case Left(error) => break(Left(error))
        }
      } else c match {
        case '\\' => escaped = true; p += 1
        case '"' => 
          pos = p + 1
          col += p - start + 1
          utf16Pos += pos - start
          break(Right(Token.StringLiteral(chars, pos(start, p + 1))))
        case _ =>
          chars :+= StringChar(c.toString, pos(p, p + 1))
          p += 1
      }
    }
    err("Unterminated string literal", start)
  }

  private def parseSym(start: Int): Either[ParseError, Token] = {
    val sym = read(c => c.isLetterOrDigit || c == '_')
    if (sym.isEmpty) err("Empty symbol literal", start)
    else tok(Token.SymbolLiteral(sym, _), start)
  }

  private def parseNum(start: Int): Either[ParseError, Token] = {
    // Handle hex or binary
    if (start + 1 < text.length && text(start) == '0') {
      text(start + 1) match {
        case 'x' => 
          pos = start + 2
          val hex = read(c => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f'))
          if (hex.isEmpty) return err("Expected hex digits after '0x'", start + 2)
          return tok(Token.IntegerLiteral(s"0x$hex", _), start)
        case 'b' => 
          pos = start + 2
          val bin = read(c => c == '0' || c == '1')
          if (bin.isEmpty) return err("Expected binary digits after '0b'", start + 2)
          return tok(Token.IntegerLiteral(s"0b$bin", _), start)
        case _ => // Continue to decimal
      }
    }

    // Handle regular numbers
    val sb = new StringBuilder(text(start).toString)
    pos = start + 1; col += 1; utf16Pos += 1
    
    // Append remaining digits
    sb.append(read(_.isDigit))
    
    var rational = false
    // Parse decimal point and fraction
    if (pos < text.length && text(pos) == '.') {
      rational = true
      sb.append('.')
      pos += 1; col += 1; utf16Pos += 1
      sb.append(read(_.isDigit))
    }

    // Parse exponent
    if (pos < text.length && (text(pos) == 'e' || text(pos) == 'E')) {
      rational = true
      sb.append(text(pos))
      pos += 1; col += 1; utf16Pos += 1

      if (pos < text.length && (text(pos) == '+' || text(pos) == '-')) {
        sb.append(text(pos))
        pos += 1; col += 1; utf16Pos += 1
      }

      val expDigits = read(_.isDigit)
      if (expDigits.isEmpty) return err("Expected digits after exponent", pos)
      sb.append(expDigits)
    }
    
    val num = sb.toString
    if (rational) tok(Token.RationalLiteral(num, _), start)
    else tok(Token.IntegerLiteral(num, _), start)
  }

  private def parseIdent(initial: String, start: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (pos < text.length && isIdentPart(pos)) {
      val c = text.codePointAt(pos)
      val len = Character.charCount(c)
      sb.append(text.substring(pos, pos + len))
      pos += len; col += 1
      utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)
    }
    tok(Token.Identifier(Vector(StringChar(sb.toString, pos(start, pos))), _), start)
  }

  private def isIdentPart(p: Int): Boolean = {
    if (p >= text.length) return false
    val c = text.codePointAt(p)
    if (Character.isSupplementaryCodePoint(c)) isIdentifierPart(c)
    else isIdentifierPart(c.toChar.toInt)
  }

  private def parseOp(initial: String, start: Int): Either[ParseError, Token] = {
    // Handle comments
    if (initial == "/" && pos < text.length && text(pos) == '/') {
      pos += 1; col += 1; utf16Pos += 1 // Skip the second '/'
      val comment = read(_ != '\n')
      return tok(Token.Comment(comment, _), start)
    }

    val op = initial + read(c => isOperatorSymbol(c.toInt))
    tok(Token.Operator(op, _), start)
  }
}
