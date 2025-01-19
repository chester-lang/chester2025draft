package chester.error

import chester.utils.doc.*

// TODO: print correct compiler internal error message, also scala builtin assert/require

inline def unreachable(message: String | ToDoc = ""): Nothing = {
  val msg = message match {
    case msg: String => msg
    case msg: ToDoc  => StringPrinter.render(msg)(using PrettierOptions.Default)
  }
  throw new AssertionError(msg)
}
