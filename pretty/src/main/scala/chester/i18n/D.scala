package chester.i18n

import chester.utils.doc.{<>, Doc, PrettierOptions, ToDoc}
import scala.language.implicitConversions

trait D {
  def d(args: ToDoc*)(using options: PrettierOptions): Doc
}

implicit def d(sc: StringContext): D = new D {
  def d(args: ToDoc*)(using PrettierOptions): Doc = {
    val parts = sc.parts.iterator
    val argsIterator = args.iterator
    var docBuilder = Doc.empty

    while (parts.hasNext) {
      val part = parts.next()
      if (part.nonEmpty) {
        docBuilder = docBuilder <> Doc.text(part)
      }
      if (argsIterator.hasNext) {
        docBuilder = docBuilder <> argsIterator.next().toDoc
      }
    }

    Doc.group(docBuilder)
  }
}
