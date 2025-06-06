package chester.i18n

import chester.utils.doc.{<>, Doc, DocConf, ToDoc}

import scala.language.implicitConversions

trait D {
  def d(args: ToDoc*)(using options: DocConf): Doc
}

implicit def d(sc: StringContext): D = new D {
  def d(args: ToDoc*)(using DocConf): Doc = {
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

trait DT {
  def dt(args: ToDoc*)(using options: DocConf): Doc
}

implicit def dt(sc: StringContext): DT = new DT {
  def dt(args: ToDoc*)(using DocConf): Doc =
    throw new UnsupportedOperationException("TODO: implement i18n doc template support")
}
