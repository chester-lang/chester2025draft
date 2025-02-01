package chester.readerv2

import chester.error.Pos

case class StringChar(text: String, pos: Pos)

sealed trait Token {
  def pos: Pos
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def isRecoveryPoint: Boolean = this match {
    case Token.RParen(_) | Token.RBracket(_) | Token.RBrace(_) | Token.Semicolon(_) | Token.Comma(_) | Token.EOF(_) => true
    case _ => false
  }
}

object Token {
  case class LParen(pos: Pos) extends Token
  case class RParen(pos: Pos) extends Token
  case class LBracket(pos: Pos) extends Token
  case class RBracket(pos: Pos) extends Token
  case class LBrace(pos: Pos) extends Token
  case class RBrace(pos: Pos) extends Token
  case class Comma(pos: Pos) extends Token
  case class Semicolon(pos: Pos) extends Token
  case class Equal(pos: Pos) extends Token
  case class Colon(pos: Pos) extends Token
  case class Dot(pos: Pos) extends Token
  case class At(pos: Pos) extends Token
  case class EOF(pos: Pos) extends Token
  case class Whitespace(pos: Pos) extends Token { override def isWhitespace = true }
  case class Comment(text: String, pos: Pos) extends Token { override def isComment = true }
  case class IntegerLiteral(value: String, pos: Pos) extends Token
  case class RationalLiteral(value: String, pos: Pos) extends Token
  case class StringLiteral(value: Vector[StringChar], pos: Pos) extends Token
  case class SymbolLiteral(value: String, pos: Pos) extends Token
  case class Identifier(parts: Vector[StringChar], pos: Pos) extends Token
  case class Operator(value: String, pos: Pos) extends Token
}
