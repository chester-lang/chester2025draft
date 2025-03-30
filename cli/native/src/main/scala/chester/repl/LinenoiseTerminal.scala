package chester.repl

import cats.Id
import chester.utils.term.InputStatus.*
import chester.utils.term.*
import linenoise.facade

class LinenoiseTerminal(init: TerminalInit) extends InTerminal[Id] {
  private var history: Vector[String] = Vector()
  private val historyFile = init.historyFile

  if (historyFile.isDefined) {
    loadHistory()
  }

  private def loadHistory(): Unit =
    historyFile.foreach(filename => facade.loadHistory(filename))

  private def saveHistory(): Unit =
    historyFile.foreach(filename => facade.saveHistory(filename))

  override def writeln(line: fansi.Str): Unit =
    println(line.render)

  override def readline(info: TerminalInfo): ReadLineResult = {
    var prompt = info.defaultPrompt
    var continue = true
    var result: ReadLineResult = EndOfFile
    var currentInputs: String = ""

    while (continue) {
      val line = facade.prompt(prompt.render).getOrElse(null)

      if (line == null) {
        continue = false
        result = EndOfFile
      } else if (line.forall(_.isWhitespace)) {
        // Ignore empty lines
      } else {
        if (currentInputs.isEmpty) {
          currentInputs = line
        } else {
          currentInputs += "\n" + line
        }

        facade.addHistory(line)
        val status = info.checkInputStatus(currentInputs)

        status match {
          case Complete =>
            saveHistory()
            history = history :+ currentInputs
            result = LineRead(currentInputs)
            continue = false
          case Incomplete =>
            prompt = info.continuationPrompt
          case Error(message) =>
            result = StatusError(message)
            continue = false
        }

        if (!continue) {
          currentInputs = ""
        }
      }
    }
    result
  }

  def close(): Unit =
    saveHistory()

  override def getHistory: Seq[String] = history
}

object LinenoiseTerminal extends Terminal[Id] {
  override def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Id] ?=> T
  ): T = {
    val terminal = new LinenoiseTerminal(init)
    try
      block(using terminal)
    finally
      terminal.close()
  }
}
