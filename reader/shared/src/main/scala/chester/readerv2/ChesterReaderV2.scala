package chester.readerv2
import chester.reader.{ParseError, ParserSource, Source}
import chester.syntax.concrete.Expr
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import chester.utils.WithUTF16
import chester.reader.FileNameAndContent
import chester.error.Pos

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

  /** Parses an expression string with offsets, typically used for REPL input.
    *
    * @param sourceName
    *   Name to associate with the source (e.g., "repl").
    * @param content
    *   The actual string content to parse.
    * @param linesOffset
    *   Line number offset from the beginning of the logical file.
    * @param posOffset
    *   Character position offset (Unicode and UTF-16) from the beginning.
    * @param ignoreLocation
    *   If true, source location information is not recorded.
    * @return
    *   Either a ParseError or the successfully parsed expression.
    */
  def parseExprWithOffset(
      sourceName: String,
      content: String,
      linesOffset: Int :| Positive0,
      posOffset: WithUTF16,
      ignoreLocation: Boolean = false
  ): Either[ParseError, Expr] = {
    val source = Source(
      FileNameAndContent(sourceName, content),
      linesOffset = linesOffset,
      posOffset = posOffset
    )
    // V2 reader setup might involve error handling during lexing itself
    try {
      val tokenizer = new Lexer(source)
      val tokens = tokenizer.tokenize().toVector
      // Check for lexer errors first
      tokens.collectFirst { case Left(err) => err } match {
        case Some(lexError) => Left(lexError)
        case None =>
          val readerState = ReaderState(tokens, 0)
          val reader = new ReaderV2(readerState, source, ignoreLocation)
          reader.parseExpr()
      }
    } catch {
      // Catch potential exceptions during lexer/parser initialization or execution
      // Although ReaderV2 is designed to return Either, safeguard against unexpected throws.
      case e: Exception => Left(ParseError(s"Unexpected error during parsing: ${e.getMessage}", Pos.zero))
    }
  }

  // Add other parsing methods here with the same pattern
}
