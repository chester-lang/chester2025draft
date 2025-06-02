package chester.targets.ts

import chester.syntax.{Tree, TreeMap}
import upickle.default.*

sealed trait Expr extends Tree[Expr] derives ReadWriter {
  def meta: Option[Meta] = None
}

case object Void0Expr extends Expr {
  override def toString: String = "void 0"

  override type ThisTree = Void0Expr.type

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Expr = ???
}
