package chester.backend.ts

import chester.utils.doc.{Doc, DocConf, ToDoc}
import upickle.default.*
case class Toplevel(stmts: Seq[TSStmt]) extends ToDoc derives ReadWriter {
  override def toDoc(using options: DocConf): Doc = Doc.mkDoc(stmts, sep = Doc.hardline)
}
