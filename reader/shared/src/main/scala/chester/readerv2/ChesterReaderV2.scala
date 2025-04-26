package chester.readerv2
import chester.reader.{FileNameAndContent, ParseError, Source}
import chester.syntax.concrete.Expr

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: FileNameAndContent, ignoreLocation: Boolean = false): LexerV2 = {
    val sourceOffset = Source(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val initialState = LexerState(tokens.toVector, 0)
    new LexerV2(initialState, sourceOffset, ignoreLocation)
  }

  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    val lexer = setupLexer(source)
    lexer.parseExpr()
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    val lexer = setupLexer(source)
    lexer.parseExprList()
  }

  def parseTopLevel(source: FileNameAndContent, ignoreLocation: Boolean = false): Either[ParseError, Expr] = {
    val lexer = setupLexer(source, ignoreLocation)
    lexer.parseTopLevel()
  }

  // Add other parsing methods here with the same pattern
}
