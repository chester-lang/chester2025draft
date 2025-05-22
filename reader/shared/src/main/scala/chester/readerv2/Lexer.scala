package chester.readerv2

import chester.error.{Pos, Span, SpanInFile}
import chester.reader.{ParseError, Source}
import chester.utils.{Nat, WithUTF16, asInt}
import chester.syntax.IdentifierRules.{isIdentifierFirst, isIdentifierPart, isOperatorSymbol}
import chester.i18n.*

import scala.util.{Try, boundary}
import scala.util.boundary.break

type TokenStream = LazyList[Either[ParseError, Token]]

object Lexer {
  def apply(sourceOffset: Source): Lexer = new Lexer(sourceOffset)
  private val escapes = Map('n' -> "\n", 't' -> "\t", 'r' -> "\r", '"' -> "\"", '\\' -> "\\", 'b' -> "\b", 'f' -> "\f")
  private val tokens: Map[Char, Span => Token] = Map(
    '(' -> Token.LParen.apply,
    ')' -> Token.RParen.apply,
    '[' -> Token.LBracket.apply,
    ']' -> Token.RBracket.apply,
    '{' -> Token.LBrace.apply,
    '}' -> Token.RBrace.apply,
    ',' -> Token.Comma.apply,
    ';' -> Token.Semicolon.apply,
    '.' -> Token.Dot.apply,
    '@' -> Token.At.apply
  )
}

class Lexer(source: Source) {
  import Lexer.*

  private val text = source.readContent.getOrElse("")
  private var pos, line, col, utf16Pos = 0

  def tokenize(): TokenStream = LazyList.unfold(false)(done =>
    if (done) None
    else if (pos >= text.length) Some((Right(Token.EOF(mkPos(0, 0))), true))
    else Some((nextToken, false))
  )

  private def mkPos(start: Int, end: Int = pos) = source.offset.add(
    Span(
      source,
      SpanInFile(
        Pos(
          WithUTF16(Nat(start), Nat(text.substring(0, start).length)),
          Nat(line),
          WithUTF16(Nat(0), Nat(col))
        ),
        Pos(
          WithUTF16(Nat(end), Nat(text.substring(0, end).length)),
          Nat(line),
          WithUTF16(Nat(end - start), Nat(text.substring(start, end).length))
        )
      )
    )
  )

  private def err(msg: String, p: Int) = Left(ParseError(msg, Some(mkPos(p))))
  private def tok[T <: Token](f: Span => T, start: Int) = Right(f(mkPos(start)))

  private def consume(pred: Char => Boolean = _ => true): String = {
    val start = pos
    while (pos < text.length && pred(text(pos))) {
      if (text(pos) == '\n') { line += 1; col = 0 }
      else col += 1
      pos += 1; utf16Pos += 1
    }
    text.substring(start, pos)
  }

  private def nextToken: Either[ParseError, Token] = {
    val wsStart = pos
    val whitespaceText = consume(_.isWhitespace)
    if (whitespaceText.nonEmpty) {
      val hasNewline = whitespaceText.contains('\n')
      return Right(Token.Whitespace(mkPos(wsStart, pos), hasNewline))
    }

    if (pos >= text.length) return Right(Token.EOF(mkPos(0, 0)))

    val start = pos
    val c = text.codePointAt(pos)
    val len = Character.charCount(c)
    pos += len; col += 1; utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)

    if (Character.isSupplementaryCodePoint(c)) {
      if (isIdentifierFirst(c)) parseIdent(String.valueOf(Character.toChars(c)), start)
      else err(t"Unexpected character: ${String.valueOf(Character.toChars(c))}", start)
    } else
      c.toChar match {
        case '#'                            => tok(Token.Hash.apply, start)
        case c if tokens.contains(c)        => tok(tokens(c), start)
        case '"'                            => parseStr(start)
        case '\''                           => tok(Token.SymbolLiteral(consume(c => c.isLetterOrDigit || c == '_'), _), start)
        case d if d.isDigit                 => parseNum(start)
        case a if a.isLetter || a == '_'    => parseIdent(a.toString, start)
        case o if isOperatorSymbol(o.asInt) => parseOp(o.toString, start)
        case x                              => err(t"Unexpected character: $x", start)
      }
  }

  private def parseEscape(start: Int): Either[ParseError, (String, Int)] =
    if (start >= text.length) err("Unexpected end of input in escape sequence", start - 1)
    else
      text(start) match {
        case c if escapes.contains(c) => Right((escapes(c), start + 1))
        case 'u' if start + 4 < text.length =>
          val hex = text.substring(start + 1, start + 5)
          Try(Integer.parseInt(hex, 16))
            .map(cp => (new String(Character.toChars(cp)), start + 5))
            .toEither
            .left
            .map(_ => ParseError(t"Invalid Unicode escape \\u$hex", Some(mkPos(start - 1, start + 5))))
        case 'x' if start + 2 < text.length =>
          val hex = text.substring(start + 1, start + 3)
          Try(Integer.parseInt(hex, 16))
            .map(v => (v.toChar.toString, start + 3))
            .toEither
            .left
            .map(_ => ParseError(t"Invalid hex escape \\x$hex", Some(mkPos(start - 1, start + 3))))
        case c if c >= '0' && c <= '7' =>
          val end = (start + 1 to Math.min(start + 3, text.length))
            .takeWhile(i => i < text.length && text(i) >= '0' && text(i) <= '7')
            .lastOption
            .getOrElse(start + 1)
          Try(Integer.parseInt(text.substring(start, end), 8))
            .withFilter(_ <= 0xff)
            .map(v => (v.toChar.toString, end))
            .toEither
            .left
            .map(_ => ParseError(t"Invalid octal escape \\${text.substring(start, end)}", Some(mkPos(start - 1, end))))
        case c => Right((c.toString, start + 1))
      }

  private def parseStr(start: Int): Either[ParseError, Token] = boundary {
    var p = start + 1
    var chars = Vector.empty[StringChar]
    var escaped = false

    while (p < text.length) {
      val c = text(p)
      if (escaped) {
        parseEscape(p) match {
          case Right((str, next)) =>
            chars :+= StringChar(str, mkPos(p - 1, next))
            escaped = false
            p = next
          case Left(error) => break(Left(error))
        }
      } else
        c match {
          case '\\' => escaped = true; p += 1
          case '"' =>
            pos = p + 1
            col += p - start + 1
            utf16Pos += pos - start
            break(Right(Token.StringLiteral(chars, mkPos(start, p + 1))))
          case _ =>
            chars :+= StringChar(c.toString, mkPos(p, p + 1))
            p += 1
        }
    }
    err("Unterminated string literal", start)
  }

  private def parseNum(start: Int): Either[ParseError, Token] = {
    if (start + 1 < text.length && text(start) == '0') {
      text(start + 1) match {
        case 'x' =>
          pos = start + 2
          val hex = consume(c => c.isDigit || ('a' <= c.toLower && c.toLower <= 'f'))
          if (hex.isEmpty) return err("Expected hex digits after '0x'", start + 2)
          return tok(Token.IntegerLiteral(t"0x$hex", _), start)
        case 'b' =>
          pos = start + 2
          val bin = consume(c => c == '0' || c == '1')
          if (bin.isEmpty) return err("Expected binary digits after '0b'", start + 2)
          return tok(Token.IntegerLiteral(t"0b$bin", _), start)
        case _ => // Continue to decimal
      }
    }

    val sb = new StringBuilder(text(start).toString)
    pos = start + 1; col += 1; utf16Pos += 1
    sb.append(consume(_.isDigit))

    var rational = false
    if (pos < text.length && text(pos) == '.') {
      rational = true
      sb.append('.'); pos += 1; col += 1; utf16Pos += 1
      sb.append(consume(_.isDigit))
    }

    if (pos < text.length && (text(pos) == 'e' || text(pos) == 'E')) {
      rational = true
      sb.append(text(pos)); pos += 1; col += 1; utf16Pos += 1
      if (pos < text.length && (text(pos) == '+' || text(pos) == '-')) {
        sb.append(text(pos)); pos += 1; col += 1; utf16Pos += 1
      }
      val expDigits = consume(_.isDigit)
      if (expDigits.isEmpty) return err("Expected digits after exponent", pos)
      sb.append(expDigits)
    }

    tok(if (rational) Token.RationalLiteral(sb.toString, _) else Token.IntegerLiteral(sb.toString, _), start)
  }

  private def parseIdent(initial: String, start: Int): Either[ParseError, Token] = {
    val sb = new StringBuilder(initial)
    while (
      pos < text.length && (
        if (Character.isSupplementaryCodePoint(text.codePointAt(pos)))
          isIdentifierPart(text.codePointAt(pos))
        else
          text.charAt(pos).isLetterOrDigit || text.charAt(pos) == '_' || isIdentifierPart(text.codePointAt(pos))
      )
    ) {
      val c = text.codePointAt(pos)
      val len = Character.charCount(c)
      sb.append(text.substring(pos, pos + len))
      pos += len; col += 1; utf16Pos += (if (Character.isSupplementaryCodePoint(c)) 2 else 1)
    }
    tok(Token.Identifier(Vector(StringChar(sb.toString, mkPos(start, pos))), _), start)
  }

  private def parseOp(initial: String, start: Int): Either[ParseError, Token] = {
    if (initial == "/" && pos < text.length && text(pos) == '/') {
      pos += 1; col += 1; utf16Pos += 1
      return tok(Token.Comment(consume(_ != '\n'), _), start)
    }
    val opText = initial + consume(c => isOperatorSymbol(c.asInt))
    tok(Token.Identifier(Vector(StringChar(opText, mkPos(start, pos))), _), start)
  }
}
