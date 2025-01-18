package linenoise

import linenoise.all._

import scala.scalanative.unsafe._

object facade {

  /** Prompts the user for input using the provided `prompt` string. Returns `Option[String]`.
    */
  def prompt(prompt: String): Option[String] = Zone {
    val cPrompt = toCString(prompt)
    val cInput = linenoise(cPrompt)
    if (cInput == null) None
    else Some(fromCString(cInput))
  }

  /** Adds a line to the history. Returns `true` on success, `false` on failure.
    */
  def addHistory(line: String): Boolean = Zone {
    val cLine = toCString(line)
    linenoiseHistoryAdd(cLine) == 0
  }

  /** Saves the current history to a file. Returns `true` on success, `false` on failure.
    */
  def saveHistory(filename: String): Boolean = Zone {
    val cFilename = toCString(filename)
    linenoiseHistorySave(cFilename) == 0
  }

  /** Loads history from a file. Returns `true` on success, `false` on failure.
    */
  def loadHistory(filename: String): Boolean = Zone {
    val cFilename = toCString(filename)
    linenoiseHistoryLoad(cFilename) == 0
  }

  /** Clears the history. */
  def clearHistory(): Unit = linenoiseHistoryFree()

  /** Sets the maximum number of lines in the history. */
  def setMaxHistoryLen(len: Int): Unit = linenoiseHistorySetMaxLen(len)

  /** Gets the current maximum number of lines in the history. */
  def getMaxHistoryLen(): Int = linenoiseHistoryGetMaxLen()

  /** Clears the terminal screen. */
  def clearScreen(): Unit = linenoiseClearScreen()

  /** Returns the number of display columns in the current terminal. */
  def getTerminalColumns(): Int = linenoiseColumns()

  /** Enables or disables multiline mode. */
  def setMultiline(enabled: Boolean): Unit = linenoiseSetMultiLine(
    if (enabled) 1 else 0
  )

  /** Sets a completion callback function. */
  // TODO

  /** Adds a completion string to the list of completions. */
  def addCompletion(
      completions: Ptr[linenoiseCompletions],
      completion: String
  ): Unit = Zone {
    val cCompletion = toCString(completion)
    linenoiseAddCompletion(completions, cCompletion)
  }

}
