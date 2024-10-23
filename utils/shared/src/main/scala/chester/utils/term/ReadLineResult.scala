package chester.utils.term

sealed trait ReadLineResult

case class LineRead(line: String) extends ReadLineResult

case class StatusError(message: String) extends ReadLineResult

case object UserInterrupted extends ReadLineResult

case object EndOfFile extends ReadLineResult
