package chester.readerv2
import chester.reader.{FileNameAndContent, ParseError, SourceOffset}
import chester.syntax.concrete.Expr

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: FileNameAndContent, ignoreLocation: Boolean = false): (LexerV2, LexerState) = {
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(sourceOffset, ignoreLocation)
    val initialState = LexerState(tokens.toVector, 0)
    (lexer, initialState)
  }

  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    val (lexer, state) = setupLexer(source)
    lexer.parseExpr(state).map(_._1)
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    val (lexer, state) = setupLexer(source)
    lexer.parseExprList(state).map(_._1)
  }

  // Add other parsing methods here with the same pattern
}
