package chester.targets.ts

import chester.syntax.{Tree, TreeMap}
import chester.utils.doc.*
import upickle.default.*

sealed trait TSExpr extends Tree[TSExpr] with ToDoc derives ReadWriter {
  def meta: Option[Meta]
}

case class Void0Expr(meta: Option[Meta] = None) extends TSExpr {
  override type ThisTree = Void0Expr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): Void0Expr = this

  override def toDoc(using options: PrettierOptions): Doc = Doc.text("(void 0)")
}

case class DoubleExpr(value: Double, meta: Option[Meta] = None) extends TSExpr {
  override def toString: String = value.toString

  override type ThisTree = DoubleExpr

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): DoubleExpr = this

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(value.toString)
}

sealed trait TSStmt extends TSExpr derives ReadWriter {
  override type ThisTree <: TSStmt
}

case class EmptyStmt(meta: Option[Meta] = None) extends TSStmt {
  override type ThisTree = EmptyStmt

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): EmptyStmt = this

  override def toDoc(using options: PrettierOptions): Doc = ";"
}

case class ConstStmt(name: String, ty: Option[TSExpr], value: TSExpr, meta: Option[Meta] = None) extends TSStmt {

  override type ThisTree = ConstStmt

  override def descent(f: TSExpr => TSExpr, g: TreeMap[TSExpr]): ConstStmt = copy(
    ty = ty.map(f),
    value = f(value)
  )

  override def toDoc(using options: PrettierOptions): Doc = ty match {
    case Some(tyExpr) =>
      "const" <+> name <+>
        ":" <+> tyExpr <+>
        "=" <+> value <+> ";"
    case None =>
      "const" <+> name <+>
        "=" <+> value <+> ";"
  }
}
