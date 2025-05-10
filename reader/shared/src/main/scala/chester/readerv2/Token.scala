package chester.readerv2

import chester.error.SourcePos
import chester.i18n.*

case class StringChar(text: String, sourcePos: SourcePos)

sealed trait Token extends Product with Serializable {
  def sourcePos: SourcePos
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def containsNewline: Boolean = false
  def tokenType: String
}
object Token {
  case class LParen(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"left parenthesis '('"
  }
  case class RParen(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"right parenthesis ')'"
  }
  case class LBracket(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"left bracket '['"
  }
  case class RBracket(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"right bracket ']"
  }
  case class LBrace(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"left brace '{'"
  }
  case class RBrace(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"right brace '}'"
  }
  case class Comma(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"comma ','"
  }
  case class Semicolon(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"semicolon ';'"
  }
  case class Dot(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"dot '.'"
  }
  case class At(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"at '@'"
  }
  case class EOF(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"end of file"
  }
  case class Whitespace(sourcePos: SourcePos, hasNewline: Boolean = false) extends Token {
    override def isWhitespace = true
    override def containsNewline: Boolean = hasNewline
    override def tokenType: String = t"whitespace"
  }
  case class Comment(text: String, sourcePos: SourcePos) extends Token {
    override def isComment = true
    override def tokenType: String = t"comment"
  }
  case class IntegerLiteral(value: String, sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"integer literal"
  }
  case class RationalLiteral(value: String, sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"rational literal"
  }
  case class StringLiteral(value: Vector[StringChar], sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"string literal"
  }
  case class SymbolLiteral(value: String, sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"symbol literal"
  }
  case class Identifier(parts: Vector[StringChar], sourcePos: SourcePos) extends Token {
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
  case class Hash(sourcePos: SourcePos) extends Token {
    override def tokenType: String = t"hash '#' "
  }
}
