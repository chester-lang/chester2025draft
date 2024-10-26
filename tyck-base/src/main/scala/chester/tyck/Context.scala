package chester.tyck

import chester.syntax.*
import chester.syntax.accociativity.OperatorsContext
import chester.syntax.core.*
import chester.tyck.api.SymbolCollector
import chester.uniqid.UniqidOf

case class TyAndVal(
                     ty: Term,
                     value: Term
                   ) {}

object TyAndVal {}

/** for pure values only like let and def. record is not included */
case class ContextItem(
                        name: Name,
                        uniqId: UniqidOf[ReferenceCall],
                        ref: ReferenceCall,
                        ty: Term,
                        reference: Option[SymbolCollector] = None
                      )
object ContextItem {}
case class Imports()

object Imports {
  val Empty: Imports = Imports()
}

case class Context(
                    map: Map[Name, UniqidOf[ReferenceCall]] = Map.empty[Name, UniqidOf[ReferenceCall]], // empty[...] are needed because compiler bugs
                    contextItems: Map[UniqidOf[ReferenceCall], ContextItem] =
                    Map.empty[UniqidOf[ReferenceCall], ContextItem], // empty[...] are needed because compiler bugs
                    knownMap: Map[UniqidOf[ReferenceCall], TyAndVal] = Map.empty[UniqidOf[ReferenceCall], TyAndVal], // empty[...] are needed because compiler bugs
                    typeDefinitionNames: Map[Name, UniqidOf[TypeDefinition]] = Map.empty,
                    typeDefinitions: Map[UniqidOf[TypeDefinition], TypeDefinition] = Map.empty,
                    imports: Imports = Imports.Empty,
                    loadedModules: LoadedModules = LoadedModules.Empty,
                    operators: OperatorsContext = OperatorsContext.Default,
                    currentModule: ModuleRef = DefaultModule
                  ) {
  def updateModule(module: ModuleRef): Context = copy(currentModule = module)

  def getKnown(x: ReferenceCall): Option[TyAndVal] =
    knownMap.get(x.uniqId.asInstanceOf[UniqidOf[ReferenceCall]])

  def get(id: Name): Option[ContextItem] =
    map.get(id).flatMap(uniqId => contextItems.get(uniqId))

  def knownAdd(id: UniqidOf[ReferenceCall], y: TyAndVal): Context =
    knownAdd(Seq(id -> y))

  def knownAdd(
                seq: Seq[(UniqidOf[ReferenceCall], TyAndVal)]
              ): Context = {
    val newKnownMap = seq.foldLeft(knownMap) { (acc, item) =>
      assert(!acc.contains(item._1), s"Duplicate key ${item._1}")
      acc + item
    }
    copy(knownMap = newKnownMap)
  }

  def add(item: ContextItem): Context = add(Seq(item))

  def add(seq: Seq[ContextItem]): Context = {
    val newMap = seq.foldLeft(map) { (acc, item) =>
      acc + (item.name -> item.uniqId)
    }
    val newContextItems = seq.foldLeft(contextItems) { (acc, item) =>
      require(!acc.contains(item.uniqId), s"Duplicate key ${item.uniqId}")
      acc + (item.uniqId -> item)
    }
    copy(map = newMap, contextItems = newContextItems)
  }
  def addTypeDefinition(typeDef: TypeDefinition): Context = {
    copy(
      typeDefinitionNames = typeDefinitionNames + (typeDef.name -> typeDef.uniqId),
      typeDefinitions = typeDefinitions + (typeDef.uniqId -> typeDef)
    )
  }

  def getTypeDefinition(name: Name): Option[TypeDefinition] = {
    typeDefinitionNames.get(name).flatMap(typeDefinitions.get)
  }

  def getTypeDefinitionById(id: UniqidOf[TypeDefinition]): Option[TypeDefinition] = {
    typeDefinitions.get(id)
  }
}

object Context {}
