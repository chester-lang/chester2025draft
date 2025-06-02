package chester.targets.ts

import upickle.default.*
case class Toplevel(stmts: Seq[TSStmt]) derives ReadWriter
