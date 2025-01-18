package chester.site

import chester.js._
import chester.repl.REPLEngine
import chester.utils.env.{BrowserEnv, Environment}
import chester.utils.io._
import chester.utils.io.impl.given
import chester.utils.term._
import typings.xtermPty.mod.Slave
import typings.xtermReadline.mod.Readline
import typings.xtermXterm.mod.Terminal

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("startRepl")
def startRepl(terminal: Terminal): js.Promise[Unit] = {
  XtermTerminal(terminal)
    .runTerminal(
      TerminalInit.Default, {
        implicit val env: Environment = BrowserEnv
        REPLEngine
      }
    )
    .toJSPromise
}

@JSExportTopLevel("startReplPty")
def startReplPty(pty: Slave): js.Promise[Unit] = {
  XtermPty(pty)
    .runTerminal(
      TerminalInit.Default, {
        implicit val env: Environment = BrowserEnv
        REPLEngine
      }
    )
    .toJSPromise
}

@JSExportTopLevel("startReplReadline")
def startReplReadline(rl: Readline): js.Promise[Unit] = {
  XtermReadline(rl)
    .runTerminal(
      TerminalInit.Default, {
        implicit val env: Environment = BrowserEnv
        REPLEngine
      }
    )
    .toJSPromise
}

inline private def runFileFuture(
    content: String,
    lightMode: Boolean
): Future[String] = {
  Future.successful(runFileTopLevel(content, lightMode))
}

@JSExportTopLevel("runFile")
def runFile(content: String, lightMode: Boolean): js.Promise[String] = {
  runFileFuture(content, lightMode).toJSPromise
}
