package chester

import org.mozilla.javascript.Context
import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.GeneratedClassLoader
import org.mozilla.javascript.NativeFunction

object Js4Jvm {
  // Initialize the Rhino context
  private val context: Context = Context.enter()
  val c = new chester.ChesterJs()
}
