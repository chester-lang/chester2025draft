package chester.utils

/** Global debug utilities and flags for Chester.
  */
object Debug {

  /** Debug categories for specifying what type of debug output to enable.
    */
  enum DebugCategory {
    case Cell // For cell filling debug info
    case Tyck // For type checker debug info
    case Reducer // For reducer debug info
    case UnionSubtyping // For union subtyping debug info
    case UnionMatching // For union matching debug info
    case Literals // For literal type handling debug info
    case Identifiers // For identifier handling debug info
    case MethodCalls // For method call handling debug info
    case StringArgs // For string arguments handling debug info
    case TraitMatching // For trait implementation matching
  }

  import DebugCategory._

  // Debug flags
  var enabledCategories: Set[DebugCategory] = Set.empty

  // Environment variable based flags - initialized at startup
  // These respect the existing environment variables for backwards compatibility
  val ENV_DEBUG_ENABLED: Boolean = sys.env.get("ENV_DEBUG").isDefined
  private val envDebugMap = Map(
    "ENV_DEBUG_UNION_SUBTYPING" -> UnionSubtyping,
    "ENV_DEBUG_UNION_MATCHING" -> UnionMatching,
    "ENV_DEBUG_LITERALS" -> Literals,
    "ENV_DEBUG_IDENTIFIERS" -> Identifiers,
    "ENV_DEBUG_METHOD_CALLS" -> MethodCalls,
    "ENV_DEBUG_STRING_ARGS" -> StringArgs
  )

  // Initialize from environment variables
  {
    // Enable categories from specific env vars
    for ((envVar, category) <- envDebugMap)
      if (sys.env.get(envVar).isDefined || ENV_DEBUG_ENABLED) {
        enabledCategories += category
      }
  }

  // Enable/disable debug for specific categories
  def enable(category: DebugCategory): Unit =
    enabledCategories += category

  def disable(category: DebugCategory): Unit =
    enabledCategories -= category

  def isEnabled(category: DebugCategory): Boolean =
    enabledCategories.contains(category)

  // Enable/disable multiple categories at once
  def enableAll(categories: DebugCategory*): Unit =
    enabledCategories ++= categories

  def disableAll(categories: DebugCategory*): Unit =
    enabledCategories --= categories

  // Reset all debug flags
  def disableAllCategories(): Unit =
    enabledCategories = Set.empty

  // Debug print method that respects debug flags
  def debugPrint(category: DebugCategory, message: => String): Unit =
    if (isEnabled(category)) {
      println(s"[DEBUG:$category] $message")
    }

  // Print a call stack trace for debugging
  def printCallStack(category: DebugCategory, depth: Int = 15): Unit =
    if (isEnabled(category)) {
      println(s"[DEBUG:$category] Call stack:")
      Thread
        .currentThread()
        .getStackTrace()
        .drop(2)
        .take(depth)
        .foreach(element =>
          println(s"[DEBUG:$category]   at ${element.getClassName}.${element.getMethodName}(${element.getFileName}:${element.getLineNumber})")
        )
    }
}
