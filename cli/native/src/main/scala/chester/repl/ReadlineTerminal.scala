package chester.repl

import cats.Id
import chester.utils.term.InputStatus._
import chester.utils.term._
import readline.facade

class ReadlineTerminal(init: TerminalInit) extends InTerminal[Id] {
  private var history: Vector[String] = Vector()
  private val historyFile = init.historyFile
  private var historyExists: Int = 0

  if (historyFile.isDefined) {
    readHistory()
  }

  // Method for reading history
  private def readHistory(): Unit = {
    historyFile.foreach { file =>
      historyExists = facade.read_history(file)
    }
  }

  // Method for writing history
  private def writeHistory(): Unit = {
    historyFile.foreach { file =>
      if (historyExists == 0) {
        facade.append_history(1, file)
      } else {
        historyExists = 0
        facade.write_history(file)
      }
    }
  }

  override def writeln(line: fansi.Str): Unit = {
    println(line.render)
  }

  override def readline(info: TerminalInfo): ReadLineResult = {
    var prompt = info.defaultPrompt
    var continue = true
    var result: ReadLineResult = EndOfFile
    var currentInputs: String = ""

    while (continue) {
      val line = facade.readline(prompt.render)

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

        facade.add_history(
          line
        ) // GNU Readline can only handle one line entry in history
        val status = info.checkInputStatus(currentInputs)

        status match {
          case Complete =>
            val prev = facade.history_get(
              facade.history_base + facade.history_length - 1
            )
            if (prev == null || prev != line) {
              writeHistory() // Manage history based on existence
            }
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

  def close(): Unit = {
    writeHistory()
  }

  override def getHistory: Seq[String] = history
}

object ReadlineTerminal extends Terminal[Id] {
  override def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Id] ?=> T
  ): T = {
    val terminal = new ReadlineTerminal(init)
    try {
      block(using terminal)
    } finally {
      terminal.close()
    }
  }
}
