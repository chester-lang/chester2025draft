package chester.syntax

import chester.utils.*
import chester.utils.doc.{ToDoc, *}
import upickle.default.*

type Name = String

inline def Name(inline name: String): Name = name

@deprecated("Use ModuleRef")
type UnresolvedID = Vector[Name]

@deprecated("Use ModuleRef")
type QualifiedIDString = Vector[Name]

extension (x: QualifiedIDString) {
  @deprecated("Use ModuleRef")
  def name: Name = x.last
}

@deprecated("Use ModuleRef")
object QualifiedIDString {
  def from(id: Name*): QualifiedIDString = id.toVector
}

implicit val ModuleRefRW: ReadWriter[ModuleRef] =
  readwriter[Vector[Name]].bimap(_.xs, ModuleRef(_))

/** nonempty */
case class ModuleRef(xs: Vector[Name]) extends AnyVal with ToDoc {
  override def toDoc(implicit options: PrettierOptions): Doc =
    Doc.text(xs.mkString("."))
}

val BuiltinModule = ModuleRef(Vector("_builtin"))

val DefaultModule = ModuleRef(Vector("_default"))

case class AbsoluteRef(module: ModuleRef, id: Name) extends ToDoc derives ReadWriter {
  def name: Name = id
  override def toDoc(implicit options: PrettierOptions): Doc =
    module.toDoc <> Doc.text(".") <> id.toDoc
}
