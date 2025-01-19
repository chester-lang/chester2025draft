package chester.utils.io

import cats.Id
import chester.repl.*
import chester.utils.term.*
import com.eed3si9n.ifdef.*

@ifndef("readline")
given DefaultTerminal: Terminal[Id] =
  LinenoiseTerminal // new SimpleTerminalFactory
@ifdef("readline")
given DefaultTerminal: Terminal[Id] = ReadlineTerminal
