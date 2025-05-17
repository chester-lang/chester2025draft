package chester.readerv2

import chester.error.Span
import chester.i18n.*

case class StringChar(text: String, sourcePos: Span)

sealed trait Token extends Product with Serializable {
  def sourcePos: Span
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def containsNewline: Boolean = false
  def tokenType: String
}
object Token {
  case class LParen(sourcePos: Span) extends Token {
    override def tokenType: String = t"left parenthesis '('"
  }
  case class RParen(sourcePos: Span) extends Token {
    override def tokenType: String = t"right parenthesis ')'"
  }
  case class LBracket(sourcePos: Span) extends Token {
    override def tokenType: String = t"left bracket '['"
  }
  case class RBracket(sourcePos: Span) extends Token {
    override def tokenType: String = t"right bracket ']"
  }
  case class LBrace(sourcePos: Span) extends Token {
    override def tokenType: String = t"left brace '{'"
  }
  case class RBrace(sourcePos: Span) extends Token {
    override def tokenType: String = t"right brace '}'"
  }
  case class Comma(sourcePos: Span) extends Token {
    override def tokenType: String = t"comma ','"
  }
  case class Semicolon(sourcePos: Span) extends Token {
    override def tokenType: String = t"semicolon ';'"
  }
  case class Dot(sourcePos: Span) extends Token {
    override def tokenType: String = t"dot '.'"
  }
  case class At(sourcePos: Span) extends Token {
    override def tokenType: String = t"at '@'"
  }
  case class EOF(sourcePos: Span) extends Token {
    override def tokenType: String = t"end of file"
  }
  case class Whitespace(sourcePos: Span, hasNewline: Boolean = false) extends Token {
    override def isWhitespace = true
    override def containsNewline: Boolean = hasNewline
    override def tokenType: String = t"whitespace"
  }
  case class Comment(text: String, sourcePos: Span) extends Token {
    override def isComment = true
    override def tokenType: String = t"comment"
  }
  case class IntegerLiteral(value: String, sourcePos: Span) extends Token {
    override def tokenType: String = t"integer literal"
  }
  case class RationalLiteral(value: String, sourcePos: Span) extends Token {
    override def tokenType: String = t"rational literal"
  }
  case class StringLiteral(value: Vector[StringChar], sourcePos: Span) extends Token {
    override def tokenType: String = t"string literal"
  }
  case class SymbolLiteral(value: String, sourcePos: Span) extends Token {
    override def tokenType: String = t"symbol literal"
  }
  case class Identifier(parts: Vector[StringChar], sourcePos: Span) extends Token {
    override def tokenType: String = t"identifier"

    /** Returns the text representation of this identifier */
    def toStr: String = parts.map(_.text).mkString

    /** Returns true if this identifier represents an operator according to the language rules */
    def isOperator: Boolean = {
      if (parts.isEmpty) return false
      chester.syntax.IdentifierRules.strIsOperator(toStr)
    }

    /** Returns the text representation of this identifier */
    def text: String = toStr
  }
  case class Hash(sourcePos: Span) extends Token {
    override def tokenType: String = t"hash '#' "
  }
}
