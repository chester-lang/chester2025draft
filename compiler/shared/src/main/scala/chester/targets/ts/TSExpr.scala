package chester.targets.ts

import chester.syntax.{Tree, TreeMap}
import upickle.default.*

sealed trait TSExpr extends Tree[TSExpr] derives ReadWriter {
  def meta: Option[Meta] = None
}

case class Void0Expr(meta: Option[Meta] = None) extends TSExpr {
  override def toString: String = "void 0"

  override type ThisTree = Void0Expr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): TSExpr = this
}

case class DoubleExpr(value: Double, meta: Option[Meta] = None) extends TSExpr {
  override def toString: String = value.toString

  override type ThisTree = DoubleExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): TSExpr = this
}