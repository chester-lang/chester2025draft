package chester.targets.ts

import upickle.default.*

import scala.language.implicitConversions

sealed trait Stmt derives ReadWriter {
  def meta: Option[Meta] = None
}

case object EmptyStmt extends Stmt {
  override def toString: String = ";"
}