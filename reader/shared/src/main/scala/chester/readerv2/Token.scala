package chester.readerv2

import chester.error.*
import chester.i18n.*
import chester.reader.codepointToString

case class StringChar(text: String, span: Span) {
  require(
    text.length == 1 || (text.length == 2 && Character.isHighSurrogate(text.charAt(0)) && Character.isLowSurrogate(text.charAt(1))),
    "StringChar must represent a single UTF-16 character or a valid surrogate pair in UTF-16"
  )
}

object StringChar {
  def apply(codePoint: Int, span: Span): StringChar = {
    new StringChar(codepointToString(codePoint), span)
  }
}

sealed trait Token extends SpanRequired {
  def span: Span
  def isWhitespace: Boolean = false
  def isComment: Boolean = false
  def containsNewline: Boolean = false
  def tokenType: String
}
object Token {
  case class LParen(span: Span) extends Token {
    override def tokenType: String = t"left parenthesis '('"
  }
  case class RParen(span: Span) extends Token {
    override def tokenType: String = t"right parenthesis ')'"
  }
  case class LBracket(span: Span) extends Token {
    override def tokenType: String = t"left bracket '['"
  }
  case class RBracket(span: Span) extends Token {
    override def tokenType: String = t"right bracket ']"
  }
  case class LBrace(span: Span) extends Token {
    override def tokenType: String = t"left brace '{'"
  }
  case class RBrace(span: Span) extends Token {
    override def tokenType: String = t"right brace '}'"
  }
  case class Comma(span: Span) extends Token {
    override def tokenType: String = t"comma ','"
  }
  case class Semicolon(span: Span) extends Token {
    override def tokenType: String = t"semicolon ';'"
  }
  case class Dot(span: Span) extends Token {
    override def tokenType: String = t"dot '.'"
  }
  case class At(span: Span) extends Token {
    override def tokenType: String = t"at '@'"
  }
  case class EOF(span: Span) extends Token {
    override def tokenType: String = t"end of file"
  }
  case class Whitespace(span: Span, hasNewline: Boolean = false) extends Token {
    override def isWhitespace = true
    override def containsNewline: Boolean = hasNewline
    override def tokenType: String = t"whitespace"
  }
  case class Comment(text: String, span: Span) extends Token {
    override def isComment = true
    override def tokenType: String = t"comment"
  }
  case class IntegerLiteral(value: String, span: Span) extends Token {
    override def tokenType: String = t"integer literal"
  }
  case class RationalLiteral(value: String, span: Span) extends Token {
    override def tokenType: String = t"rational literal"
  }
  case class StringLiteral(value: Vector[StringChar], span: Span) extends Token {
    override def tokenType: String = t"string literal"
  }
  case class SymbolLiteral(value: String, span: Span) extends Token {
    override def tokenType: String = t"symbol literal"
  }
  case class Identifier(parts: Vector[StringChar], span: Span) extends Token {
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
  case class Hash(span: Span) extends Token {
    override def tokenType: String = t"hash '#' "
  }
}
