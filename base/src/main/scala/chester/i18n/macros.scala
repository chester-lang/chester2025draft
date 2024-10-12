package chester.i18n

import scala.quoted.*
import scala.io.Source
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

trait T {
  def t(args: Any*)(using lang: Language): String
}

implicit inline def t(inline sc: StringContext): T = ${ tMacro('sc) }

private def tMacro(sc: Expr[StringContext])(using Quotes): Expr[T] = {
  import quotes.reflect.*

  '{
    new T {
      def t(args: Any*)(using lang: Language): String = {
        // Convert StringContext to a template key using existing function
        val key = Template.stringContextToString(${ sc })

        // Fetch the translation using existing TranslationTable
        val translation = TranslationTable.get(lang, key)

        // Apply arguments to the template using existing function
        Template.applyTemplate(translation, args.toVector)
      }
    }
  }
}
