package chester.idea

import com.intellij.lang.Language

// object can't extends com.intellij.lang.Language otherwise intellij will complaint about internal api usages
class ChesterLanguage extends Language("Chester")

object ChesterLanguage {
  val INSTANCE = new ChesterLanguage
}