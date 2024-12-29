package chester.error

import chester.utils.doc.{Doc, PrettierOptions, ToDoc}
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
