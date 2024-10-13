package chester.syntax.concrete

import chester.syntax.QualifiedIDString
import chester.syntax.concrete.*

import scala.collection.immutable.HashMap

@deprecated("Create new module representation")
type FileName = String
@deprecated("Create new module representation")
type Hash = Int

@deprecated("Create new module representation")
case class ResolvingBlock(statements: Vector[Stmt], expr: Option[Expr])

@deprecated("Create new module representation")
case class ResolvingModuleFile(
    id: QualifiedIDString,
    fileName: FileName,
    sourceHash: Option[Hash] = None,
    content: Option[ResolvingBlock] = None,
    tycked: Option[Nothing] = None
) {
  require(content.isDefined || tycked.isDefined)
}

@deprecated("Create new module representation")
object ResolvingModuleFile {
  def apply(
      id: QualifiedIDString,
      fileName: FileName,
      sourceHash: Option[Hash] = None,
      content: Option[ResolvingBlock] = None,
      tycked: Option[Nothing] = None
  ): ResolvingModuleFile = {
    new ResolvingModuleFile(id, fileName, sourceHash, content, tycked)
  }
  def apply(
      id: QualifiedIDString,
      fileName: FileName,
      content: ResolvingBlock
  ): ResolvingModuleFile = {
    new ResolvingModuleFile(id, fileName, content = Some(content))
  }
}

@deprecated("Create new module representation")
case class ResolvingModule(
    id: QualifiedIDString,
    resolving: Vector[ResolvingModuleFile]
)

@deprecated("Create new module representation")
case class ResolvingModules(
    modules: HashMap[QualifiedIDString, ResolvingModule]
) extends AnyVal {
  def getOption(id: QualifiedIDString): Option[ResolvingModule] =
    modules.get(id)
  def addModuleFile(
      id: QualifiedIDString,
      moduleFile: ResolvingModuleFile
  ): ResolvingModules = {
    require(moduleFile.id == id)
    ???
  }
}

@deprecated("Create new module representation")
object ResolvingBlock {
  def fromParsed(block: Block): ResolvingBlock = {
    ResolvingBlock(
      statements = block.heads.map(x => ExprStmt(x, x.meta)),
      expr = block.tail
    )
  }
}

@deprecated("Create new module representation")
object ResolvingModule {}

@deprecated("Create new module representation")
object ResolvingModules {
  val Empty: ResolvingModules = ResolvingModules(HashMap.empty)
}
