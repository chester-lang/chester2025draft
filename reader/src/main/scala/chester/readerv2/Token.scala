package chester.readerv2

import chester.error.{Pos, SourcePos}

case class StringChar(text: String, sourcePos: SourcePos)

sealed trait Token {
  def sourcePos: SourcePos
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def isRecoveryPoint: Boolean = this match {
    case Token.RParen(_) | Token.RBracket(_) | Token.RBrace(_) | Token.Semicolon(_) | Token.Comma(_) | Token.EOF(_) => true
    case _ => false
  }
}

object Token {
  case class LParen(sourcePos: SourcePos) extends Token
  case class RParen(sourcePos: SourcePos) extends Token
  case class LBracket(sourcePos: SourcePos) extends Token
  case class RBracket(sourcePos: SourcePos) extends Token
  case class LBrace(sourcePos: SourcePos) extends Token
  case class RBrace(sourcePos: SourcePos) extends Token
  case class Comma(sourcePos: SourcePos) extends Token
  case class Semicolon(sourcePos: SourcePos) extends Token
  case class Colon(sourcePos: SourcePos) extends Token
  case class Dot(sourcePos: SourcePos) extends Token
  case class At(sourcePos: SourcePos) extends Token
  case class EOF(sourcePos: SourcePos) extends Token
  case class Whitespace(sourcePos: SourcePos) extends Token { override def isWhitespace = true }
  case class Comment(text: String, sourcePos: SourcePos) extends Token { override def isComment = true }
  case class IntegerLiteral(value: String, sourcePos: SourcePos) extends Token
  case class RationalLiteral(value: String, sourcePos: SourcePos) extends Token
  case class StringLiteral(value: Vector[StringChar], sourcePos: SourcePos) extends Token
  case class SymbolLiteral(value: String, sourcePos: SourcePos) extends Token
  case class Identifier(parts: Vector[StringChar], sourcePos: SourcePos) extends Token
  case class Operator(value: String, sourcePos: SourcePos) extends Token
}
