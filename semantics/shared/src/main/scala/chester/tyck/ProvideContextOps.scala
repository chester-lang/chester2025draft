package chester.tyck

import chester.syntax.core.*
import chester.utils.propagator.*
import chester.utils.cell.*

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
