package chester.reader

import upickle.default.*
import chester.error.*
import chester.utils.WithUTF16
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.upickle.given
import chester.utils.doc.{Doc, PrettierOptions}
import spire.math.UInt
import chester.utils.impls.uintRW

case class ParseError(message: String, pos: Pos) extends Problem {
  override def severity: Problem.Severity = Problem.Severity.Error
  override def stage: Problem.Stage = Problem.Stage.PARSE

  override def toDoc(using PrettierOptions): Doc = Doc.text(message)
  def sourcePos: Option[SourcePos] = None // TODO
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
    if (impl == null) Left(ParseError("No FilePathImpl provided", Pos.zero))
    else impl.readContent(fileName)
}

// Explicitly import the given instance for UInt ReadWriter
import chester.utils.impls.uintRW

// TODO: maybe column offset for the first line also
case class Source(
    source: ParserSource,
    linesOffset: spire.math.UInt = UInt(0),
    posOffset: WithUTF16 = WithUTF16.Zero
) derives ReadWriter {
  def fileName: String = source.fileName

  def readContent: Either[ParseError, String] = source.readContent
}
