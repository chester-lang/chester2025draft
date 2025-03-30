package chester.js

import chester.utils.io.*
import chester.utils.io.impl.given
import chester.utils.term.*
import typings.std.global.setTimeout
import typings.xtermReadline.mod.Readline

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*

def setTimeoutThen: Future[Unit] = {
  val promise = Promise[Unit]()
  setTimeout(() => promise.success(()), 0)
  promise.future
}

final class InXtermReadline(rl: Readline) extends InTerminalNoHistory[Future] {
  override inline def writeln(line: fansi.Str): Future[Unit] = {
    rl.println(line.render)
    setTimeoutThen
  }

  override inline def readALine(prompt: fansi.Str): Future[String] =
    rl.read(prompt.render)
}

case class XtermReadline(rl: Readline) extends Terminal[Future] {
  def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Future] ?=> Future[T]
  ): Future[T] =
    block(using InXtermReadline(rl))
}
