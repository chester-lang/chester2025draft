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

implicit val NamespaceRW: ReadWriter[Namespace] =
  readwriter[Name].bimap(_.x, Namespace(_))

/** when it is needed to have different version of the same library etc */
case class Namespace(x: Name) extends AnyVal with ToDoc {
  override def toDoc(using options: PrettierOptions): Doc = Doc.text(x)
}

object Namespace {
  val Default = Namespace("default")
}

/** nonempty */
case class ModuleRef(xs: Vector[Name], namespace: Namespace = Namespace.Default) extends ToDoc derives ReadWriter {
  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(xs.mkString("."))
}

val BuiltinModule = ModuleRef(Vector("_builtin"))

val DefaultModule = ModuleRef(Vector("_default"))

case class AbsoluteRef(module: ModuleRef, id: Name) extends ToDoc derives ReadWriter {
  def name: Name = id
  override def toDoc(using options: PrettierOptions): Doc =
    module.toDoc <> Doc.text(".") <> id.toDoc
}
