package chester.docs

import chester.js._

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("chesterRunFile")
def chesterRunFile(content: String, lightMode: Boolean): String = {
  runFileTopLevel(content, lightMode)
}
