package chester.tyck

import chester.syntax.*
import chester.syntax.core.*
import chester.tyck.PreludeBuiltin.BuiltinItem
import chester.uniqid.*
import chester.utils.propagator.*
import chester.utils.cell.*

implicit class LocalCtxOps(ignored: Context.type) {
  val default: Context = {
    val items = PreludeBuiltin.builtinItems.map(ContextItem.builtin)
    val map = items.map(item => item._2.name -> item._2.uniqId).toMap
    val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
    val knownMap: Map[UniqidOf[ReferenceCall], TyAndVal] = items
      .map(item => item._2.uniqId -> item._1)
      .toMap
      .asInstanceOf[Map[UniqidOf[ReferenceCall], TyAndVal]]
    Context(map, contextItems, knownMap)
  }
}

implicit class ContextItemObject(private val ignored: ContextItem.type) extends AnyVal {
  def builtin(
      item: BuiltinItem
  ): (TyAndVal, ContextItem) = {
    val varId = Uniqid.generate[ToplevelV]
    val name = ToplevelV(AbsoluteRef(BuiltinModule, item.id), item.ty, varId, None)
    val ty1 = item.ty
    (
      TyAndVal(ty1, item.value),
      ContextItem(item.id, varId, name, ty1)
    )
  }
}

trait ProvideContextOps extends ProvideCellId with ElaboraterBase {

  implicit class TyAndValnuoOpsss(ignored: TyAndVal.type) {
    def create(ty: Term, value: Term)(using
        StateOps[TyckOps]
    ): TyAndVal =
      new TyAndVal(toTerm(literal(ty)), toTerm(literal(value)))

    def create()(using state: StateOps[TyckOps]): TyAndVal =
      new TyAndVal(toTerm(state.addCell(OnceCellContent[Term]())), toTerm(state.addCell(OnceCellContent[Term]())))
  }

  extension (context: ContextItem) {
    def tyId(using StateOps[TyckOps]): CellId[Term] = toId(context.ty)

    def tyTerm(using StateOps[TyckOps]): Term = toTerm(context.ty)
  }

  implicit class TyAndValOps(tyandval: TyAndVal) {
    def tyId(using StateOps[TyckOps]): CellId[Term] = toId(tyandval.ty)

    def valueId(using StateOps[TyckOps]): CellId[Term] = toId(tyandval.value)

    def tyTerm(using StateOps[TyckOps]): Term = toTerm(tyandval.ty)

    def valueTerm(using StateOps[TyckOps]): Term = toTerm(tyandval.value)

  }

}
