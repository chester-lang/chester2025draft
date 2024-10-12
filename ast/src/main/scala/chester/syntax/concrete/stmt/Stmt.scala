package chester.syntax.concrete.stmt

import chester.error._
import chester.syntax.Name
import upickle.default._

@deprecated("Create new module representation")
type ModuleName = ModuleNameQualified

@deprecated("Create new module representation")
case class ModuleNameQualified(ids: Vector[Name]) derives ReadWriter

object ModuleName {
  def builtin: ModuleNameQualified = ModuleNameQualified(Vector())
}

@deprecated("Create new module representation")
case class QualifiedID(
    component: ModuleName,
    name: Name,
    sourcePos: Option[SourcePos] = None
) extends WithPos derives ReadWriter

@deprecated("Create new module representation")
object QualifiedID {
  def builtin(name: String): QualifiedID = QualifiedID(ModuleName.builtin, name)
}
