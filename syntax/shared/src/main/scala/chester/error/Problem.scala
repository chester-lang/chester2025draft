package chester.error

import chester.utils.doc.*
import upickle.default.*

object Problem {
  enum Stage derives ReadWriter {
    case TYCK, PARSE, OTHER
  }

  enum Severity derives ReadWriter {
    case Error, Goal, Warning, Info
  }
}

trait WithServerity extends Any {
  def severity: Problem.Severity

  final def isError: Boolean = severity == Problem.Severity.Error
}

case class DescriptionElement(doc: ToDoc, sourcePos: Option[SourcePos]) extends WithPos

case class FullDescription(begin: ToDoc, explanations: Vector[DescriptionElement], end: ToDoc)

trait Problem extends ToDoc with WithPos with WithServerity {
  def stage: Problem.Stage
  // TODO: use this
  def fullDescription: Option[FullDescription] = None
}

private case class ProblemSer(
    stage: Problem.Stage,
    severity: Problem.Severity,
    message: Doc,
    sourcePos: Option[SourcePos]
) extends Problem derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc = message
}

private object ProblemSer {
  def from(problem: Problem): ProblemSer = ProblemSer(
    problem.stage,
    problem.severity,
    problem.toDoc(using PrettierOptions.Default),
    problem.sourcePos
  )
}

object ProblemUpickle {
  implicit val problemRW: ReadWriter[Problem] =
    readwriter[ProblemSer].bimap(ProblemSer.from, x => x)
}

extension (p: Problem) {
  def renderDoc(using options: PrettierOptions, sourceReader: SourceReader): Doc = {
    p.fullDescription match {
      case Some(desc) => renderFullDescription(desc)(using options, sourceReader)
      case None => renderToDocWithSource(p)(using options, sourceReader)
    }
  }
}

private def renderFullDescription(desc: FullDescription)(using options: PrettierOptions, sourceReader: SourceReader): Doc = {
  val beginDoc = desc.begin.toDoc
  val explanationsDoc = desc.explanations.map { elem =>
    val elemDoc = elem.doc.toDoc
    elem.sourcePos.flatMap(sourceReader.apply) match {
      case Some(source) => elemDoc </> Doc.text(source)
      case None => elemDoc
    }
  }
  val endDoc = desc.end.toDoc

  Doc.concat(
    beginDoc,
    Doc.line,
    ssep(explanationsDoc, Doc.line),
    Doc.line,
    endDoc
  )
}

private def renderToDocWithSource(p: Problem)(using options: PrettierOptions, sourceReader: SourceReader): Doc = {
  val baseDoc = p.toDoc
  p.sourcePos.flatMap(sourceReader.apply) match {
    case Some(source) => 
      baseDoc <> Doc.line <> Doc.text(source)
    case None => 
      baseDoc
  }
}

case class SourceReader(readSource: SourcePos => Option[String]) {
  def apply(pos: SourcePos): Option[String] = readSource(pos)
}

object SourceReader {
  def fromFileContent(content: FileContent): SourceReader = {
    SourceReader { pos =>
      pos.getLinesInRange.map { lines =>
        lines.map { case (_, line) => line }.mkString("\n")
      }
    }
  }

  def empty: SourceReader = SourceReader(_ => None)
}
