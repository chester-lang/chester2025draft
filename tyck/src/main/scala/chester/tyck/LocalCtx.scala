package chester.tyck

import chester.syntax.accociativity.OperatorsContext
import chester.syntax.*
import chester.syntax.core.*
import chester.syntax.Name
import chester.tyck.BuiltIn.BuiltinItem
import chester.tyck.api.SymbolCollector
import chester.utils.propagator.*
import chester.uniqid.*

trait ProvideCtx extends ProvideCellId with ElaboraterBase {

  case class ContextItem(
      name: Name,
      uniqId: UniqIdOf[? <: MaybeVarCall],
      ref: MaybeVarCall,
      ty: CellIdOr[Term],
      reference: Option[SymbolCollector] = None
  ) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(ty)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(ty)
  }

  object ContextItem {
    def builtin[Ck](
        item: BuiltinItem
    )(using state: StateAbility[Ck]): (TyAndVal, ContextItem) = {
      val varId = UniqId.generate[ToplevelV]
      val name = ToplevelV(AbsoluteRef(BuiltinModule, item.id), item.ty, varId)
      val ty1 = state.toId(item.ty)
      (
        TyAndVal(ty1, state.toId(item.value)),
        ContextItem(item.id, varId, name, ty1)
      )
    }
  }

  case class TyAndVal(
      ty: CellIdOr[Term],
      value: CellIdOr[Term]
  ) {
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(ty)

    def valueId(using state: StateAbility[Tyck]): CellId[Term] = toId(value)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(ty)

    def valueTerm(using state: StateAbility[Tyck]): Term = toTerm(value)
  }

  object TyAndVal {
    def create[Ck](ty: Term, value: Term)(using
        state: StateAbility[Ck]
    ): TyAndVal = {
      TyAndVal(literal(ty), literal(value))
    }

    def create[Ck]()(using state: StateAbility[Ck]): TyAndVal = {
      TyAndVal(state.addCell(OnceCell[Term]()), state.addCell(OnceCell[Term]()))
    }
  }

  case class Imports()

  object Imports {
    val Empty: Imports = Imports()
  }
  case class RecordDefinition(
      name: Name,
      fields: Vector[FieldTerm],
      extendsSymbol: Option[Name], // Store the symbol from the extends clause
      id: UniqIdOf[LocalV],
      ty: CellId[Term]
  )
  case class LocalCtx(
      map: Map[Name, UniqIdOf[? <: MaybeVarCall]] = Map.empty[Name, UniqIdOf[? <: MaybeVarCall]],
      contextItems: Map[UniqIdOf[? <: MaybeVarCall], ContextItem] = Map.empty[UniqIdOf[? <: MaybeVarCall], ContextItem],
      knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] = Map.empty[UniqIdOf[? <: MaybeVarCall], TyAndVal],
      recordDefinitions: Map[Name, RecordDefinition] = Map.empty, // New field for records
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
    def addRecordDefinition(recordDef: RecordDefinition): LocalCtx = {
      copy(recordDefinitions = recordDefinitions + (recordDef.name -> recordDef))
    }

    // Method to get a record definition by name
    def getRecordDefinition(name: Name): Option[RecordDefinition] = {
      recordDefinitions.get(name)
    }
  }

  object LocalCtx {
    def default[Ck](using state: StateAbility[Ck]): LocalCtx = {
      val items = BuiltIn.builtinItems.map(ContextItem.builtin)
      val map = items.map(item => item._2.name -> item._2.uniqId).toMap
      val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
      val knownMap: Map[UniqIdOf[? <: MaybeVarCall], TyAndVal] = items
        .map(item => item._2.uniqId -> item._1)
        .toMap
        .asInstanceOf[Map[UniqIdOf[? <: MaybeVarCall], TyAndVal]]
      LocalCtx(map, contextItems, knownMap)
    }
  }
}
