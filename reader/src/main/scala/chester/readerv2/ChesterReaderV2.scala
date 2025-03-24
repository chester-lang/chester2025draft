package chester.readerv2

import chester.error.Reporter
import chester.reader.{FileNameAndContent, ParseError, SourceOffset}
import chester.syntax.concrete.Expr

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: FileNameAndContent, ignoreLocation: Boolean = false): LexerV2 = {
    given reporter: Reporter[ParseError] = new Reporter[ParseError] {
      def apply(error: ParseError): Unit = {}
      def const(e: ParseError): Reporter[ParseError] = this
    }
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(tokens, sourceOffset, ignoreLocation)
    lexer
  }

  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    val lexer = setupLexer(source)
    lexer.parse().map(_.headOption.getOrElse(throw new RuntimeException("No expressions found")))
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    val lexer = setupLexer(source)
    lexer.parse()
  }

  // Add other parsing methods here with the same pattern
}
