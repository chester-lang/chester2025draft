package chester.idea

import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.util.IconLoader
import javax.swing.Icon

class ChesterFileType extends LanguageFileType(ChesterLanguage.INSTANCE) {
  override def getName: String = "Chester File"
  override def getDescription: String = "Chester language file"
  override def getDefaultExtension: String = "chester"
  override def getIcon: Icon =
    IconLoader.getIcon("/icons/chester-icon.png", getClass)
}

object ChesterFileType {
  val INSTANCE: ChesterFileType = new ChesterFileType()
}
