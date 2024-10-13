package chester.tyck

import chester.syntax.accociativity.OperatorsContext
import chester.syntax.*
import chester.syntax.core.*
import chester.syntax.Name
import chester.tyck.BuiltIn.BuiltinItem
import chester.tyck.api.SymbolCollector
import chester.utils.propagator.*
import chester.uniqid.*

import scala.collection.immutable.Seq as seq

trait ProvideCtx extends ProvideCellId with ElaboraterBase {

  implicit class TyAndValOpsss(ignored: TyAndVal.type) {
    def create(ty: Term, value: Term)(using
        state: StateAbility[Tyck]
    ): TyAndVal = {
      new TyAndVal(toTerm(literal(ty)), toTerm(literal(value)))
    }

    def create()(using state: StateAbility[Tyck]): TyAndVal = {
      new TyAndVal(toTerm(state.addCell(OnceCell[Term]())), toTerm(state.addCell(OnceCell[Term]())))
    }
  }

  extension (context: ContextItem) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(context.ty)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(context.ty)
  }

  implicit class ContextItemObject(ignored: ContextItem.type) {
    def builtin(
        item: BuiltinItem
    )(using state: StateAbility[Tyck]): (TyAndVal, ContextItem) = {
      val varId = UniqId.generate[ToplevelV]
      val name = ToplevelV(AbsoluteRef(BuiltinModule, item.id), item.ty, varId)
      val ty1 = state.toId(item.ty)
      (
        new TyAndVal(toTerm(ty1), item.value),
        new ContextItem(item.id, varId, name, toTerm(ty1))
      )
    }
  }

  implicit class TyAndValOps(tyandval: TyAndVal) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.ty)

    def valueId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.value)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.ty)

    def valueTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.value)

  }

  implicit class LocalCtxOps(ignored: LocalCtx.type) {
    def default(using state: StateAbility[Tyck]): LocalCtx = {
      val items = BuiltIn.builtinItems.map(ContextItem.builtin)
      val map = items.map(item => item._2.name -> item._2.uniqId).toMap
      val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
      val knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] = items
        .map(item => item._2.uniqId -> item._1)
        .toMap
        .asInstanceOf[Map[UniqIdOf[? <: MaybeVarCall], TyAndVal]]
      new LocalCtx(map, contextItems, knownMap)
    }
  }

}

case class TyAndVal(
    ty: Term,
    value: Term
) {}

object TyAndVal {}

/** for pure values only like let and def. record is not included */
case class ContextItem(
    name: Name,
    uniqId: UniqIdOf[? <: MaybeVarCall],
    ref: MaybeVarCall,
    ty: Term,
    reference: Option[SymbolCollector] = None
)
object ContextItem {}
case class Imports()

object Imports {
  val Empty: Imports = Imports()
}

case class LocalCtx(
    map: Map[Name, UniqIdOf[? <: MaybeVarCall]] = Map.empty[Name, UniqIdOf[? <: MaybeVarCall]], // empty[...] are needed because compiler bugs
    contextItems: Map[UniqIdOf[? <: MaybeVarCall], ContextItem] = Map.empty[UniqIdOf[? <: MaybeVarCall], ContextItem], // empty[...] are needed because compiler bugs
    knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] = Map.empty[UniqIdOf[? <: MaybeVarCall], TyAndVal], // empty[...] are needed because compiler bugs
     recordDefinitionNames: Map[Name, UniqIdOf[RecordStmtTerm]] = Map.empty, // Map from Name to UniqId
    recordDefinitions: Map[UniqIdOf[RecordStmtTerm], RecordStmtTerm] = Map.empty, // Map from UniqId to RecordDefinition
    imports: Imports = Imports.Empty,
    loadedModules: LoadedModules = LoadedModules.Empty,
    operators: OperatorsContext = OperatorsContext.Default,
    currentModule: ModuleRef = DefaultModule
) {
  def updateModule(module: ModuleRef): LocalCtx = copy(currentModule = module)

  def getKnown(x: MaybeVarCall): Option[TyAndVal] =
    knownMap.get(x.uniqId.asInstanceOf[UniqIdOf[? <: MaybeVarCall]])

  def get(id: Name): Option[ContextItem] =
    map.get(id).flatMap(uniqId => contextItems.get(uniqId))

  def knownAdd(id: UniqIdOf[? <: MaybeVarCall], y: TyAndVal): LocalCtx =
    knownAdd(Seq(id -> y))

  def knownAdd(
      seq: Seq[(UniqIdOf[? <: MaybeVarCall], TyAndVal)]
  ): LocalCtx = {
    val newKnownMap = seq.foldLeft(knownMap) { (acc, item) =>
      assert(!acc.contains(item._1), s"Duplicate key ${item._1}")
      acc + item
    }
    copy(knownMap = newKnownMap)
  }

  def add(item: ContextItem): LocalCtx = add(Seq(item))

  def add(seq: Seq[ContextItem]): LocalCtx = {
    val newMap = seq.foldLeft(map) { (acc, item) =>
      acc + (item.name -> item.uniqId)
    }
    val newContextItems = seq.foldLeft(contextItems) { (acc, item) =>
      require(!acc.contains(item.uniqId), s"Duplicate key ${item.uniqId}")
      acc + (item.uniqId -> item)
    }
    copy(map = newMap, contextItems = newContextItems)
  }

    // Method to add a record definition to the context
    def addRecordDefinition(recordDef: RecordStmtTerm): LocalCtx = {
        copy(
            recordDefinitionNames = recordDefinitionNames + (recordDef.name -> recordDef.uniqId),
            recordDefinitions = recordDefinitions + (recordDef.uniqId -> recordDef)
        )
    }

    // Method to get a record definition by name
    def getRecordDefinition(name: Name): Option[RecordStmtTerm] = {
        recordDefinitionNames.get(name).flatMap(recordDefinitions.get)
    }

    // (Optional) Method to get a record definition by UniqId
    def getRecordDefinitionById(id: UniqIdOf[RecordStmtTerm]): Option[RecordStmtTerm] = {
        recordDefinitions.get(id)
    }
}

object LocalCtx {}
