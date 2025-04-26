package chester.utils
import chester.i18n.*
// Import Parameter
import chester.utils.Parameter
// Import mutable Set
import scala.collection.mutable

/** Global debug utilities and flags for Chester.
  * Provides both globally mutable defaults and scoped overrides.
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


  // Globally mutable set for default enabled categories
  private val globallyEnabledCategories: mutable.Set[DebugCategory] = mutable.Set.empty

  // Map from DebugCategory to Parameter[Boolean] for SCOPED overrides.
  // Parameters are created WITHOUT a default value.
  val categoryParameters: Map[DebugCategory, Parameter[Boolean]] =
    DebugCategory.values.map(cat => cat -> Parameter[Boolean]()).toMap

  /** Sets the default enabled state for a category globally.
    */
  def setDefault(category: DebugCategory, enabled: Boolean): Unit =
    if (enabled) globallyEnabledCategories.add(category)
    else globallyEnabledCategories.remove(category)

  /** Resets all global default category settings to disabled.
    */
  def resetDefaults(): Unit =
    globallyEnabledCategories.clear()

  /** Checks if a category is enabled, considering both scoped overrides and global defaults.
    */
  def isEnabled(category: DebugCategory): Boolean =
    categoryParameters.get(category) match {
      case Some(param) =>
        // Check scoped value first using getOption
        param.getOption match {
          case Some(scopedValue) => scopedValue // Use scoped value if present (true or false)
          case None => globallyEnabledCategories.contains(category) // Fallback to global default
        }
      // Should not happen as map is exhaustive
      case None => false
    }

  /** Executes a block of code with a specific debug category enabled (scoped override).
    */
  def withCategoryEnabled[U](category: DebugCategory)(block: => U): U =
    categoryParameters.get(category) match {
      case Some(param) => param.withValue(true)(block)
      case None        => block // Should not happen as map is exhaustive
    }

  /** Executes a block of code with multiple debug categories enabled (scoped override).
    */
  def withCategoriesEnabled[U](categories: Iterable[DebugCategory])(block: => U): U = {
    categories.foldLeft(() => block) { (currentBlock, category) =>
      () => withCategoryEnabled(category)(currentBlock())
    }()
  }

  // Debug print method that respects debug flags
  def debugPrint(category: DebugCategory, message: => String): Unit =
    if (isEnabled(category)) {
      println(t"[DEBUG:$category] $message")
    }

  // Print a call stack trace for debugging
  def printCallStack(category: DebugCategory, depth: Int = 15): Unit =
    if (isEnabled(category)) {
      println(t"[DEBUG:$category] Call stack:")
      Thread
        .currentThread()
        .getStackTrace
        .slice(2, depth + 2)
        .foreach(element =>
          println(t"[DEBUG:$category]   at ${element.getClassName}.${element.getMethodName}(${element.getFileName}:${element.getLineNumber})")
        )
    }
}
