package chester

import org.graalvm.polyglot.*
import org.graalvm.polyglot.io.IOAccess

object Js4Jvm {
  // https://www.graalvm.org/latest/reference-manual/js/Modules/
  private val contextBuilder = Context
    .newBuilder("js")
    .allowIO(IOAccess.ALL)
    .option("js.esm-eval-returns-exports", "true")
    .option("engine.WarnInterpreterOnly", "false")
  lazy val context: Context = contextBuilder.build()
  private val source = Source.newBuilder("js", chester.generated.GeneratedJS.jsCode, "main.mjs").mimeType("application/javascript+module").build()
  lazy val exports: Value = context.eval(source)
}
