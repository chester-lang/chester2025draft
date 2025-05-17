package chester.reader

import upickle.default.*
import chester.error.*
import chester.utils.{Nat, WithUTF16}
import chester.utils.doc.{Doc, PrettierOptions}
import spire.math.Natural
import chester.utils.impls.naturalRW

case class ParseError(message: String, sourcePos: Option[Span] = None) extends Problem {
  override def severity: Problem.Severity = Problem.Severity.Error
  override def stage: Problem.Stage = Problem.Stage.PARSE

  override def toDoc(using PrettierOptions): Doc = Doc.text(message)
}

sealed trait ParserSource extends Product with Serializable derives ReadWriter {
  def fileName: String

  def readContent: Either[ParseError, String]
}

case class FileNameAndContent(fileName: String, content: String) extends ParserSource derives ReadWriter {
  override def readContent: Either[ParseError, String] = Right(content)
}

trait FilePathImpl {
  def readContent(fileName: String): Either[ParseError, String]

  def absolute(fileName: String): String
}

object FilePath {
  def apply(fileName: String)(using used: FilePathImpl): FilePath = {
    val path = used.absolute(fileName)
    val result = new FilePath(path)
    result.impl = used
    result
  }
}

case class FilePath private (fileName: String) extends ParserSource {
  private[chester] var impl: FilePathImpl = scala.compiletime.uninitialized
  override lazy val readContent: Either[ParseError, String] =
    if (impl == null) Left(ParseError("No FilePathImpl provided"))
    else impl.readContent(fileName)
}

// TODO: maybe column offset for the first line also
case class Source(
    source: ParserSource,
    offset: Offset = Offset.Zero
) derives ReadWriter {
  def fileName: String = source.fileName

  def readContent: Either[ParseError, String] = source.readContent
}

case class Offset(
    lineOffset: spire.math.Natural = Nat(0),
    posOffset: WithUTF16 = WithUTF16.Zero
) derives ReadWriter {

  if (lineOffset != Nat(0)) require(posOffset.nonZero)
  if (posOffset.nonZero) require(lineOffset != Nat(0))
  def add(x: Pos): Pos = Pos(index = posOffset + x.index, line = x.line + lineOffset, column = x.column)
  def add(x: SpanInFile): SpanInFile = SpanInFile(start = add(x.start), end = add(x.end))
  def add(x: Span): Span = Span(source = x.source, range = add(x.range))
}

object Offset {
  val Zero: Offset = Offset()
}
