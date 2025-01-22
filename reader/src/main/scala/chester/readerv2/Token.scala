package chester.readerv2

import chester.error.Pos
import chester.reader.ParseError

sealed trait Token {
  def pos: Pos
  def text: String
}

object Token {
  sealed trait NamePart
  case class IdentifierPart(value: Vector[Char]) extends NamePart
  case class OperatorPart(value: Vector[Char]) extends NamePart

  sealed trait StringSegment
  case class StringChars(chars: Vector[Char]) extends StringSegment
  case class StringEscape(char: Char) extends StringSegment
  case class StringInterpolation(expr: Vector[Token]) extends StringSegment

  case class Identifier(parts: Vector[NamePart], pos: Pos) extends Token {
    def text: String = parts.map {
      case IdentifierPart(chars) => chars.mkString
      case OperatorPart(chars)   => chars.mkString
    }.mkString
  }

  case class IntegerLiteral(value: BigInt, radix: Int, pos: Pos) extends Token {
    def text: String = if (radix == 10) value.toString else s"0x${value.toString(16)}"
  }

  case class RationalLiteral(value: BigDecimal, pos: Pos) extends Token {
    def text: String = value.toString
  }

  case class StringLiteral(segments: Vector[StringSegment], pos: Pos) extends Token {
    def text: String = {
      val sb = new StringBuilder("\"")
      segments.foreach {
        case StringChars(chars)        => sb.append(chars.mkString)
        case StringEscape(c)           => sb.append('\\').append(c)
        case StringInterpolation(expr) => sb.append("${").append(expr.map(_.text).mkString).append("}")
      }
      sb.append("\"").toString
    }
  }

  case class SymbolLiteral(name: String, pos: Pos) extends Token {
    def text: String = s"'$name"
  }

  // Keywords
  case class Keyword(name: String, pos: Pos) extends Token {
    def text = s"#$name"
  }

  // Operators
  case class Operator(symbol: String, pos: Pos) extends Token {
    def text = symbol
  }

  // Delimiters
  case class LParen(pos: Pos) extends Token { def text = "(" }
  case class RParen(pos: Pos) extends Token { def text = ")" }
  case class LBrace(pos: Pos) extends Token { def text = "{" }
  case class RBrace(pos: Pos) extends Token { def text = "}" }
  case class LBracket(pos: Pos) extends Token { def text = "[" }
  case class RBracket(pos: Pos) extends Token { def text = "]" }
  case class Comma(pos: Pos) extends Token { def text = "," }
  case class Dot(pos: Pos) extends Token { def text = "." }
  case class Equal(pos: Pos) extends Token { def text = "=" }
  case class Arrow(pos: Pos) extends Token { def text = "->" }

  // Comments and Whitespace
  case class SingleLineComment(content: Vector[Char], pos: Pos) extends Token {
    def text: String = s"//${content.mkString}"
  }

  case class Whitespace(chars: Vector[Char], pos: Pos) extends Token {
    def text: String = chars.mkString
  }

  case class EOF(pos: Pos) extends Token { def text = "" }
}

type TokenStream = LazyList[Either[ParseError, Token]]
