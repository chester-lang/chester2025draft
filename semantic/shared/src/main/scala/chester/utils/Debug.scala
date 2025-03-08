package chester.utils

/**
 * Global debug utilities and flags for Chester.
 */
object Debug {
  /**
   * Debug categories for specifying what type of debug output to enable.
   */
  enum DebugCategory {
    case Cell     // For cell filling debug info
    case Tyck     // For type checker debug info
    case Reducer  // For reducer debug info
  }
  
  import DebugCategory._
  
  // Debug flags
  var enabledCategories: Set[DebugCategory] = Set.empty
  
  // Enable/disable debug for specific categories
  def enable(category: DebugCategory): Unit = {
    enabledCategories += category
  }
  
  def disable(category: DebugCategory): Unit = {
    enabledCategories -= category
  }
  
  def isEnabled(category: DebugCategory): Boolean = {
    enabledCategories.contains(category)
  }
  
  // Debug print method that respects debug flags
  def debugPrint(category: DebugCategory, message: => String): Unit = {
    if (isEnabled(category)) {
      println(s"[DEBUG:${category}] $message")
    }
  }
  
  // Print a call stack trace for debugging
  def printCallStack(category: DebugCategory, depth: Int = 15): Unit = {
    if (isEnabled(category)) {
      println(s"[DEBUG:${category}] Call stack:")
      Thread.currentThread().getStackTrace().drop(2).take(depth).foreach(element => 
        println(s"[DEBUG:${category}]   at ${element.getClassName}.${element.getMethodName}(${element.getFileName}:${element.getLineNumber})"))
    }
  }
} 