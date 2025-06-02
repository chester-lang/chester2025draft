package chester.targets.ts

import upickle.default.*
case class Toplevel(stmts: Vector[TSStmt]) derives ReadWriter
