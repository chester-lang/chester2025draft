package chester.readerv1

import chester.error.*
import chester.i18n.*
import chester.reader.{FileNameAndContent, ParseError, ParserSource, Source}
import chester.syntax.concrete.*
import chester.utils.StringIndex
import fastparse.*
import chester.utils.WithUTF16
import spire.math.Natural

import scala.util.*
object ChesterReader {

  private def parseFromSource[T](
      source: ParserSource,
      parserFunc: ReaderV1 => P[T],
      ignoreLocation: Boolean = false
  ): Either[ParseError, T] =
    source.readContent.fold(
      error => Left(error),
      { content =>
        val indexer = StringIndex(content)
        parse(
          content,
          x =>
            parserFunc(
              ReaderV1(
                Source(source),
                ignoreLocation = ignoreLocation,
                defaultIndexer = Some(indexer)
              )(using x)
            )
        ) match {
          case Parsed.Success(result, _) => Right(result)
          case Parsed.Failure(_, index, extra) =>
            val pos = indexer.charIndexToLineAndColumnWithUTF16(index)
            val p = Pos(
              indexer.charIndexToWithUTF16(Natural(index)),
              pos.line,
              pos.column
            )
            Left(ParseError(t"Parsing failed: ${extra.trace().longMsg}", p))
        }
      }
    )

  def parseStatements(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, Vector[ParsedExpr]] =
    parseFromSource(source, _.statementsEntrance, ignoreLocation)

  def parseTopLevel(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, Block] =
    parseFromSource(source, _.toplevelEntrance, ignoreLocation)

  def parseExpr(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, ParsedExpr] =
    parseFromSource(source, _.exprEntrance, ignoreLocation)

  /** Parses an expression string, typically used for REPL input.
    *
    * @param sourceName
    *   Name to associate with the source (e.g., "repl").
    * @param content
    *   The actual string content to parse.
    * @param linesOffset
    *   Line number offset from the beginning of the logical file.
    * @param posOffset
    *   Character position offset (UTF-8 and UTF-16) from the beginning.
    * @return
    *   Either a ParseError or the successfully parsed expression.
    */
  def parseExprWithOffset(
      sourceName: String,
      content: String,
      linesOffset: spire.math.Natural,
      posOffset: WithUTF16
  ): Either[ParseError, ParsedExpr] = {
    val indexer = StringIndex(content)
    val source = Source(
      FileNameAndContent(sourceName, content),
      linesOffset = linesOffset,
      posOffset = posOffset
    )
    parse(
      content,
      p => ReaderV1(source)(using p).exprEntrance
    ) match {
      case Parsed.Success(expr, _)         => Right(expr)
      case Parsed.Failure(_, index, extra) =>
        // Use the indexer associated with *this* content snippet for error reporting
        val pos = indexer.charIndexToLineAndColumnWithUTF16(index)
        // Adjust the position by the global offsets
        val indexWithinContent: WithUTF16 = indexer.charIndexToWithUTF16(Natural(index))
        val finalIndex: WithUTF16 = posOffset + indexWithinContent
        val finalPos = Pos(
          finalIndex,
          linesOffset + pos.line,
          // Column is relative to the line start, but if it's the first line (pos.line == 1),
          // add the original UTF-16 offset's column component if applicable.
          // However, the existing Source logic likely handles this, let's keep it simple.
          // Revisit if column reporting seems off in multi-line REPL inputs.
          pos.column
        )
        Left(ParseError(t"Parsing failed: ${extra.trace().longMsg}", finalPos))
    }
  }

}
