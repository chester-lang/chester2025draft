package chester

import org.mozilla.javascript.{Context, Scriptable, ScriptableObject}
import _root_.chester.utils.*

// single threaded
object Js4Jvm {
  private def wrap[T](f: => T): T = {
    val oldJ = System.getProperty("java.vm.name")
    onNativeImage {
      // hack rhino to not use dynamic features
      System.setProperty("java.vm.name", "Dalvik")
    }
    println(s"vmname: ${System.getProperty("java.vm.name")}")
    try {
      val result = f
      result
    } finally {
      onNativeImage {
        System.setProperty("java.vm.name", oldJ)
      }
    }
  }
  private var context: Context = null
  private def check(): Unit = {
    if (context ne null) return
    val cx = Context.enter()
    cx.setLanguageVersion(Context.VERSION_ES6)
    cx.setInterpretedMode(true)
    context = cx
  }
  wrap { check() }
  private val scope: ScriptableObject = wrap { context.initStandardObjects() }
  private val script: org.mozilla.javascript.Script = new ChesterJs()
  private val exports: Scriptable = wrap {

    val result: Scriptable = context.newObject(scope)

    scope.put("exports", scope, result)

    script.exec(context, scope)

    result
  }
  private val test = wrap {exports.get("test", exports).asInstanceOf[org.mozilla.javascript.Function]}

  val helloFromJs: CharSequence = wrap { exports.get("helloFromJs", exports).asInstanceOf[CharSequence] }

  onNativeImageBuildTime {
    Context.exit()
    context = null
  }

  def test(x: Any): Any = wrap {
    check()
    test.call(context, exports, exports, Array(x))
  }
}
