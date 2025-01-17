package chester.tyck

import chester.syntax.*
import chester.syntax.core.*
import chester.tyck.BuiltIn.BuiltinItem
import chester.uniqid.*
import chester.utils.propagator.*

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
    def tyId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.ty)

    def valueId(using state: StateAbility[Tyck]): CellId[Term] = toId(tyandval.value)

    def tyTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.ty)

    def valueTerm(using state: StateAbility[Tyck]): Term = toTerm(tyandval.value)

  }

  implicit class LocalCtxOps(ignored: Context.type) {
    def default(using state: StateAbility[Tyck]): Context = {
      val items = BuiltIn.builtinItems.map(ContextItem.builtin)
      val map = items.map(item => item._2.name -> item._2.uniqId).toMap
      val contextItems = items.map(item => item._2.uniqId -> item._2).toMap
      val knownMap: Map[UniqidOf[ReferenceCall], TyAndVal] = items
        .map(item => item._2.uniqId -> item._1)
        .toMap
        .asInstanceOf[Map[UniqidOf[ReferenceCall], TyAndVal]]
      new Context(map, contextItems, knownMap)
    }
  }

}
