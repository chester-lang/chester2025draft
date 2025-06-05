package chester.elab

import chester.cell.{CellEffects, FixedEffectsCellContent}
import chester.syntax.core.{Effects, EffectsM, MetaTerm}
import chester.utils.HoldNotReadable
import chester.utils.elab.SolverOps

type Eff = CellEffects | EffectsM

extension (eff: Eff) {
  def toCellEffects(using SolverOps): CellEffects = eff match {
    case e: CellEffects @unchecked                                  => e
    case MetaTerm(HoldNotReadable(e: CellEffects @unchecked), meta) => e
    case effects: Effects                                           => SolverOps.addCell(FixedEffectsCellContent(effects))
  }
  def toEffectsM(using SolverOps): EffectsM = eff match {
    case eff: EffectsM                => eff
    case cell: CellEffects @unchecked => MetaTerm(HoldNotReadable(cell), meta = None)
  }
}
