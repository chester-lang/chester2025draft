package chester.readerv2

import chester.error.Pos

sealed trait Token {
  def pos: Pos
  def text: String
}

sealed trait TokenKind {
  def text: String
}

sealed trait Literal extends TokenKind
sealed trait Delimiter extends TokenKind
sealed trait Operator extends TokenKind

object TokenKind {
  sealed trait Name {
    def parts: Vector[NamePart]
  }
  
  sealed trait NamePart
  case class IdentifierPart(value: Vector[Char]) extends NamePart
  case class OperatorPart(value: Vector[Char]) extends NamePart
  
  case class Identifier(parts: Vector[NamePart]) extends TokenKind with Name {
    def text: String = parts.map {
      case IdentifierPart(chars) => chars.mkString
      case OperatorPart(chars) => chars.mkString
    }.mkString
  }
  
  case class IntegerLiteral(value: BigInt, radix: Int) extends TokenKind with Literal {
    def text: String = if (radix == 10) value.toString else s"0x${value.toString(16)}"
  }
  
  case class RationalLiteral(value: BigDecimal) extends TokenKind with Literal {
    def text: String = value.toString
  }
  
  case class StringLiteral(segments: Vector[StringSegment]) extends TokenKind with Literal {
    def text: String = s"\"${segments.map(_.text).mkString}\""
  }
  
  sealed trait StringSegment {
    def text: String
  }
  case class StringChars(chars: Vector[Char]) extends StringSegment {
    def text: String = chars.mkString
  }
  case class StringEscape(char: Char) extends StringSegment {
    def text: String = s"\\$char"
  }
  
  case class SymbolLiteral(segments: Vector[StringSegment]) extends TokenKind with Literal {
    def text: String = s"'${segments.map(_.text).mkString}"
  }
  
  // Delimiters
  case object LParen extends TokenKind with Delimiter { def text = "(" }
  case object RParen extends TokenKind with Delimiter { def text = ")" }
  case object LBrace extends TokenKind with Delimiter { def text = "{" }
  case object RBrace extends TokenKind with Delimiter { def text = "}" }
  case object LBracket extends TokenKind with Delimiter { def text = "[" }
  case object RBracket extends TokenKind with Delimiter { def text = "]" }
  
  // Operators
  case object Comma extends TokenKind with Operator { def text = "," }
  case object Dot extends TokenKind with Operator { def text = "." }
  case object Equal extends TokenKind with Operator { def text = "=" }
  case object Arrow extends TokenKind with Operator { def text = "->" }
  
  // Comments and Whitespace
  case class Comment(content: Vector[Char]) extends TokenKind {
    def text: String = s"//${content.mkString}"
  }
  
  case class Whitespace(chars: Vector[Char]) extends TokenKind {
    def text: String = chars.mkString
  }
  
  case object EOF extends TokenKind { def text = "" }
}

case class TokenWithPos(kind: TokenKind, pos: Pos) extends Token {
  def text: String = kind.text
}