package chester

import org.mozilla.javascript.{Context, GeneratedClassLoader, NativeFunction, Scriptable, ScriptableObject}
import _root_.chester.ChesterJs

object Js4Jvm {
  // Initialize the Rhino context
  private val context: Context = Context.enter()
  private val scope: ScriptableObject = context.initStandardObjects()
  private val script: org.mozilla.javascript.Script = new ChesterJs()
  val exports: Scriptable = {
    script.exec(context, scope)
    val result = scope.get("exports", scope)
    if(result == Scriptable.NOT_FOUND) {
      throw new RuntimeException("No exports found")
    }
    if(!result.isInstanceOf[Scriptable]) {
        throw new RuntimeException("Exports is not a Scriptable")
    }
    val r = result.asInstanceOf[Scriptable]
    r
  }
  val helloFromJs: CharSequence = exports.get("helloFromJs", exports).asInstanceOf[CharSequence]
}
