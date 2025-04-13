package chester.readerv2
import chester.reader.{FileNameAndContent, ParseError, SourceOffset}
import chester.syntax.concrete.Expr

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: FileNameAndContent, ignoreLocation: Boolean = false): LexerV2 = {
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val initialState = LexerState(sourceOffset, tokens.toVector, 0)
    new LexerV2(initialState, ignoreLocation)
  }

  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    val lexer = setupLexer(source)
    lexer.parseExpr()
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    val lexer = setupLexer(source)
    lexer.parseExprList()
  }
}
