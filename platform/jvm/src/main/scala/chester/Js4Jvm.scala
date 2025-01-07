package chester

import org.mozilla.javascript.{Context, Scriptable, ScriptableObject}
import _root_.chester.utils.onNativeImageBuildTime

// single threaded
object Js4Jvm {
  private var context: Context = null
  private def check(): Unit = {
    if (context ne null) return
    val cx = Context.enter()
    cx.setLanguageVersion(Context.VERSION_ES6)
    context = cx
  }
  check()
  private val scope: ScriptableObject = context.initStandardObjects()
  private val script: org.mozilla.javascript.Script = new ChesterJs()
  private val exports: Scriptable = {

    val result: Scriptable = context.newObject(scope)

    scope.put("exports", scope, result)

    script.exec(context, scope)

    result
  }
  private val test = exports.get("test", exports).asInstanceOf[org.mozilla.javascript.Function]

  val helloFromJs: CharSequence = exports.get("helloFromJs", exports).asInstanceOf[CharSequence]

  onNativeImageBuildTime {
    Context.exit()
    context = null
  }

  def test(x: Any): Any = {
    check()
    test.call(context, exports, exports, Array(x))
  }
}
