package chester

import org.mozilla.javascript.{Context, Scriptable, ScriptableObject}

// single threaded
object Js4Jvm {
  // Initialize the Rhino context
  private val context: Context = {
    val cx = Context.enter()
    cx.setLanguageVersion(Context.VERSION_ES6)
    cx
  }
  private val scope: ScriptableObject = context.initStandardObjects()
  private val script: org.mozilla.javascript.Script = new ChesterJs()
  private val exports: Scriptable = {

    val result: Scriptable = context.newObject(scope)

    scope.put("exports", scope, result)

    script.exec(context, scope)

    result
  }
  // for graalvm
  private var checked = false
  private def check(): Unit = {
    if (checked) return
    val result = Context.enter(context)
    assert(result eq context)
    checked = true
  }
  private val test = exports.get("test", exports).asInstanceOf[org.mozilla.javascript.Function]
  def test(x: Any): Any = {
    check()
    test.call(context, exports, exports, Array(x))
  }
  val helloFromJs: CharSequence = exports.get("helloFromJs", exports).asInstanceOf[CharSequence]
}
