package chester

import org.mozilla.javascript.{Context, Scriptable, ScriptableObject}

object Js4Jvm {
  // Initialize the Rhino context
  private val context: Context = {
    val cx = Context.enter()
    cx.setLanguageVersion(Context.VERSION_ES6)
    cx
  }
  private val scope: ScriptableObject = context.initStandardObjects()
  private val script: org.mozilla.javascript.Script = new ChesterJs()
  val exports: Scriptable = {

    val result: Scriptable = context.newObject(scope)

    scope.put("exports", scope, result)

    script.exec(context, scope)

    result
  }
  val helloFromJs: CharSequence = exports.get("helloFromJs", exports).asInstanceOf[CharSequence]
}
