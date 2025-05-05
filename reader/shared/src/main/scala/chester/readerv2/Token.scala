package chester.readerv2

import chester.error.SourcePos

case class StringChar(text: String, sourcePos: SourcePos)

sealed trait Token extends Product with Serializable {
  def sourcePos: SourcePos
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def containsNewline: Boolean = false
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
  case class Dot(sourcePos: SourcePos) extends Token
  case class At(sourcePos: SourcePos) extends Token
  case class EOF(sourcePos: SourcePos) extends Token
  case class Whitespace(sourcePos: SourcePos, hasNewline: Boolean = false) extends Token {
    override def isWhitespace = true
    override def containsNewline: Boolean = hasNewline
  }
  case class Comment(text: String, sourcePos: SourcePos) extends Token { override def isComment = true }
  case class IntegerLiteral(value: String, sourcePos: SourcePos) extends Token
  case class RationalLiteral(value: String, sourcePos: SourcePos) extends Token
  case class StringLiteral(value: Vector[StringChar], sourcePos: SourcePos) extends Token
  case class SymbolLiteral(value: String, sourcePos: SourcePos) extends Token
  case class Identifier(parts: Vector[StringChar], sourcePos: SourcePos) extends Token
  case class Operator(value: String, sourcePos: SourcePos) extends Token
  case class Hash(sourcePos: SourcePos) extends Token
}
