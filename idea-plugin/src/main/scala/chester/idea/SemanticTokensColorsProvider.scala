package chester.idea

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.psi.PsiFile
import com.redhat.devtools.lsp4ij.features.semanticTokens.SemanticTokensColorsProvider

class ChesterSemanticTokensColorsProvider extends SemanticTokensColorsProvider {
  override def getTextAttributesKey(
      tokenType: String,
      tokenModifiers: java.util.List[String],
      file: PsiFile
  ): TextAttributesKey = {
    // Map LSP token types and modifiers to IntelliJ TextAttributesKey
    // Return null for tokens that should not be highlighted
    null
  }
}
