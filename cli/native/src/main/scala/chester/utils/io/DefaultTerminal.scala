package chester.utils.io

import cats.Id
import chester.repl._
import chester.utils.term._
import com.eed3si9n.ifdef._

@ifndef("readline")
given DefaultTerminal: Terminal[Id] =
  LinenoiseTerminal // new SimpleTerminalFactory
@ifdef("readline")
given DefaultTerminal: Terminal[Id] = ReadlineTerminal
