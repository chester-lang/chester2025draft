package chester.js

import chester.utils.io.*
import chester.utils.io.impl.given
import chester.utils.term.*
import typings.xtermPty.mod.Slave

import java.nio.charset.StandardCharsets
import scala.concurrent.{Future, Promise}
import scala.scalajs.js

final class InXtermPty(pty: Slave) extends InTerminalNoHistory[Future] {
  override inline def writeln(line: fansi.Str): Future[Unit] = {
    pty.write(line.render + "\n")
    setTimeoutThen
  }
  override inline def readALine(prompt: fansi.Str): Future[String] = {
    val promise = Promise[String]()
    pty.write(prompt.render)
    val onReadable =
      pty.onReadable.asInstanceOf[js.Function1[js.Function0[Unit], Unit]]
    onReadable(() =>
      promise.success(
        new String(pty.read().map(_.toByte).toArray, StandardCharsets.UTF_8)
      )
    )
    promise.future
  }
}

case class XtermPty(pty: Slave) extends Terminal[Future] {
  def runTerminal[T](
      init: TerminalInit,
      block: InTerminal[Future] ?=> Future[T]
  ): Future[T] =
    block(using InXtermPty(pty))
}
