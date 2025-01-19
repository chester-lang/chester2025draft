package chester.reader

import chester.syntax.concrete._
import chester.utils.StringIndex
import fastparse._
import chester.error._
import _root_.io.github.iltotore.iron._

import scala.collection.immutable
import scala.util._
object ChesterReader {

  private def parseFromSource[T](
      source: ParserSource,
      parserFunc: ReaderInternal => P[T],
      ignoreLocation: Boolean = false
  ): Either[ParseError, T] = {
    source.readContent.fold(error => Left(error), { content => val indexer = StringIndex(content)
        parse(
          content,
          x =>
            parserFunc(
              ReaderInternal(
                SourceOffset(source),
                ignoreLocation = ignoreLocation,
                defaultIndexer = Some(indexer)
              )(using x)
            )
        ) match {
          case Parsed.Success(result, _) => Right(result)
          case Parsed.Failure(_, index, extra) =>
            val pos = indexer.charIndexToLineAndColumnWithUTF16(index)
            val p = Pos(
              indexer.charIndexToWithUTF16(index.refineUnsafe),
              pos.line,
              pos.column
            )
            Left(ParseError(s"Parsing failed: ${extra.trace().longMsg}", p))
        } })
  }

  def parseStatements(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, Vector[ParsedExpr]] = {
    parseFromSource(source, _.statementsEntrance, ignoreLocation)
  }

  def parseTopLevel(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, Block] = {
    parseFromSource(source, _.toplevelEntrance, ignoreLocation)
  }

  def parseExpr(
      source: ParserSource,
      ignoreLocation: Boolean = false
  ): Either[ParseError, ParsedExpr] = {
    parseFromSource(source, _.exprEntrance, ignoreLocation)
  }

}
