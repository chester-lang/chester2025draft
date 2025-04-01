package chester

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Entry point for Scala code that will be compiled to Lua Exports functions using JSExportTopLevel which will be available in Lua
  */
object LuaExports {

  /** Simple test function
    */
  @JSExportTopLevel("test")
  def test(): String =
    "Hello from Chester Scala code running in Lua!"

  /** Basic string manipulation example
    */
  @JSExportTopLevel("reverseString")
  def reverseString(s: String): String =
    s.reverse

  /** Numeric operation example
    */
  @JSExportTopLevel("factorial")
  def factorial(n: Int): Int =
    if (n <= 1) 1 else n * factorial(n - 1)

  /** Example of handling complex data types using JSON
    */
  @JSExportTopLevel("processData")
  def processData(jsonData: String): String =
    try {
      // In a real implementation, this would parse and process the JSON
      // For this example, we'll just return a modified version
      val prefix = "Processed: "
      prefix + jsonData
    } catch {
      case e: Exception => "Error processing data: " + e.getMessage
    }

  /** Entry point to access all chester functionality This creates a top-level Chester object that can be accessed in Lua
    */
  @JSExportTopLevel("Chester")
  val chester: js.Object = js.Dynamic.literal(
    test = test _,
    reverseString = reverseString _,
    factorial = factorial _,
    processData = processData _
  )
}
