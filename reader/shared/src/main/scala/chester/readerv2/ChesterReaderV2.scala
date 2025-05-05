package chester.readerv2
import chester.reader.{FileNameAndContent, Offset, ParseError, ParserSource, Source}
import chester.syntax.concrete.*
import chester.utils.WithUTF16

import scala.language.implicitConversions

object ChesterReaderV2 {
  // Helper method to set up tokenizer and lexer with common logic
  private def setupLexer(source: Source, ignoreLocation: Boolean = false): ReaderV2 = {
    val tokenizer = new Lexer(source)
    val tokens = tokenizer.tokenize()
    val initialState = ReaderState(tokens.toVector, 0)
    new ReaderV2(initialState, source, ignoreLocation)
  }

  def parseExpr(source: ParserSource, ignoreLocation: Boolean = false): Either[ParseError, ParsedExpr] = {
    val lexer = setupLexer(Source(source), ignoreLocation)
    lexer.parseExpr()
  }

  def parseTopLevel(source: ParserSource, ignoreLocation: Boolean = false): Either[ParseError, ParsedExpr] = {
    val lexer = setupLexer(Source(source), ignoreLocation)
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
      linesOffset: spire.math.Natural,
      posOffset: WithUTF16,
      ignoreLocation: Boolean = false
  ): Either[ParseError, ParsedExpr] = {
    val source = Source(
      FileNameAndContent(sourceName, content),
      offset = Offset(
        lineOffset = linesOffset,
        posOffset = posOffset
      )
    )
    val lexer = setupLexer(source, ignoreLocation)
    lexer.parseExpr()
  }

  // Add other parsing methods here with the same pattern
}
