package chester.tyck

import chester.syntax.*
import chester.syntax.core.*
import chester.tyck.PreludeBuiltin.BuiltinItem
import chester.uniqid.*
import chester.utils.propagator.*

trait ProvideContextOps extends ProvideCellId with ElaboraterBase {

  implicit class TyAndValnuoOpsss(ignored: TyAndVal.type) {
    def create(ty: Term, value: Term)(using
                                      StateWith[TyckSession]
    ): TyAndVal =
      new TyAndVal(toTerm(literal(ty)), toTerm(literal(value)))

    def create()(using state: StateWith[TyckSession]): TyAndVal =
      new TyAndVal(toTerm(state.addCell(OnceCell[Term]())), toTerm(state.addCell(OnceCell[Term]())))
  }

  extension (context: ContextItem) {
    def tyId(using StateWith[TyckSession]): CellId[Term] = toId(context.ty)

    def tyTerm(using StateWith[TyckSession]): Term = toTerm(context.ty)
  }

  implicit class ContextItemObject(ignored: ContextItem.type) {
    def builtin(
        item: BuiltinItem
    )(using state: StateWith[TyckSession]): (TyAndVal, ContextItem) = {
      val varId = Uniqid.generate[ToplevelV]
      val name = ToplevelV(AbsoluteRef(BuiltinModule, item.id), item.ty, varId, None)
      val ty1 = state.toId(item.ty)
      (
        new TyAndVal(toTerm(ty1), item.value),
        new ContextItem(item.id, varId, name, toTerm(ty1))
      )
    }
  }

  implicit class TyAndValOps(tyandval: TyAndVal) {
    def tyId(using StateWith[TyckSession]): CellId[Term] = toId(tyandval.ty)

    def valueId(using StateWith[TyckSession]): CellId[Term] = toId(tyandval.value)

    def tyTerm(using StateWith[TyckSession]): Term = toTerm(tyandval.ty)

    def valueTerm(using StateWith[TyckSession]): Term = toTerm(tyandval.value)

  }

  implicit class LocalCtxOps(ignored: Context.type) {
    def default(using StateWith[TyckSession]): Context = {
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

}
