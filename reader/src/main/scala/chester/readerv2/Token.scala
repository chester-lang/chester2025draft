package chester.readerv2

import chester.error.Pos
import chester.reader.ParseError

sealed trait Token {
  def pos: Pos
  def text: String
}

object Token {
  sealed trait IdentifierPart {
    def text: String
  }
  case class NormalPart(chars: Vector[Char]) extends IdentifierPart {
    override def text: String = chars.mkString
  }
  case class OpPart(chars: Vector[Char]) extends IdentifierPart {
    override def text: String = chars.mkString
  }

  sealed trait StringPart {
    def text: String
  }
  case class StringChars(chars: Vector[Char]) extends StringPart {
    override def text: String = chars.mkString
  }
  case class StringEscape(c: Char) extends StringPart {
    override def text: String = c.toString
  }
  case class StringInterpolation(tokens: Vector[Token]) extends StringPart {
    override def text: String = "${" + tokens.map(_.text).mkString + "}"
  }

  case class IntegerLiteral(value: BigInt, radix: Int, pos: Pos) extends Token {
    override def text: String = value.toString(radix)
  }
  case class RationalLiteral(value: Double, pos: Pos) extends Token {
    override def text: String = value.toString
  }
  case class StringLiteral(parts: Vector[StringPart], pos: Pos) extends Token {
    override def text: String = "\"" + parts.map(_.text).mkString + "\""
  }
  case class Identifier(parts: Vector[IdentifierPart], pos: Pos) extends Token {
    override def text: String = parts.map(_.text).mkString
  }
  case class Operator(text: String, pos: Pos) extends Token
  case class SymbolLiteral(name: String, pos: Pos) extends Token {
    override def text: String = "'" + name
  }
  case class Keyword(name: String, pos: Pos) extends Token {
    override def text: String = name
  }

  case class LParen(pos: Pos) extends Token {
    override def text: String = "("
  }
  case class RParen(pos: Pos) extends Token {
    override def text: String = ")"
  }
  case class LBrace(pos: Pos) extends Token {
    override def text: String = "{"
  }
  case class RBrace(pos: Pos) extends Token {
    override def text: String = "}"
  }
  case class LBracket(pos: Pos) extends Token {
    override def text: String = "["
  }
  case class RBracket(pos: Pos) extends Token {
    override def text: String = "]"
  }
  case class Comma(pos: Pos) extends Token {
    override def text: String = ","
  }
  case class Dot(pos: Pos) extends Token {
    override def text: String = "."
  }
  case class Colon(pos: Pos) extends Token {
    override def text: String = ":"
  }
  case class Semicolon(pos: Pos) extends Token {
    override def text: String = ";"
  }
  case class Equal(pos: Pos) extends Token {
    override def text: String = "="
  }
  case class Arrow(pos: Pos) extends Token {
    override def text: String = "->"
  }
  case class SingleLineComment(content: Vector[Char], pos: Pos) extends Token {
    override def text: String = "//" + content.mkString
  }
  case class Whitespace(chars: Vector[Char], pos: Pos) extends Token {
    override def text: String = chars.mkString
  }
  case class EOF(pos: Pos) extends Token {
    override def text: String = "<EOF>"
  }
}

type TokenStream = LazyList[Either[ParseError, Token]]
