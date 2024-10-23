package chester.syntax.core.stmt

import chester.syntax.{Name, QualifiedIDString}
import chester.syntax.core._
import chester.uniqid.*
import scala.collection.immutable.HashMap

@deprecated("deprecated")
case class TyckedSpace(modules: HashMap[QualifiedIDString, TyckedModule]) extends AnyVal

@deprecated("deprecated")
case class TyckedModule(
    id: QualifiedIDString,
    definitions: HashMap[Name, TyckedDefinitionNamed],
    lastExpr: Option[Judge]
)

@deprecated("deprecated")
sealed trait TyckedDefinition {
  def meta: Option[TermMeta]
  // def ctx: LocalCtx = ???
}

@deprecated("deprecated")
sealed trait TyckedDefinitionNamed extends TyckedDefinition {
  def name: Name
  def varId: Uniqid
}

@deprecated("deprecated")
case class TyckedExpression(judge: Judge, meta: Option[TermMeta] = None) extends TyckedDefinition

@deprecated("deprecated")
case class RecordMember()

@deprecated("deprecated")
case class TyckedRecord(
    name: Name,
    varId: Uniqid,
    members: Vector[RecordMember],
    meta: Option[TermMeta] = None
) extends TyckedDefinitionNamed

@deprecated("deprecated")
case class TyckedDef(
    name: Name,
    varId: Uniqid,
    body: Judge,
    meta: Option[TermMeta] = None
) extends TyckedDefinitionNamed
