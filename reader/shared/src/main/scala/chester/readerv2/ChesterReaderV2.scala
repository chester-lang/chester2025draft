package chester.readerv2
import chester.reader.{ParseError, Source, ParserSource}
import chester.syntax.concrete.Expr

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: ParserSource, ignoreLocation: Boolean = false): ReaderV2 = {
    val sourceOffset = Source(source)
    val tokenizer = new Lexer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val initialState = ReaderState(tokens.toVector, 0)
    new ReaderV2(initialState, sourceOffset, ignoreLocation)
  }

  def parseExpr(source: ParserSource): Either[ParseError, Expr] = {
    val lexer = setupLexer(source)
    lexer.parseExpr()
  }

  def parseExprList(source: ParserSource): Either[ParseError, Vector[Expr]] = {
    val lexer = setupLexer(source)
    lexer.parseExprList()
  }

  def parseTopLevel(source: ParserSource, ignoreLocation: Boolean = false): Either[ParseError, Expr] = {
    val lexer = setupLexer(source, ignoreLocation)
    lexer.parseTopLevel()
  }

  // Add other parsing methods here with the same pattern
}
