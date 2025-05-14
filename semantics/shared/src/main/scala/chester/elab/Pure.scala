package chester.elab

import chester.cell.CellEffects
import chester.utils.elab.*

case object Pure extends Kind {
  type Of = Pure
}

case class Pure(effects: CellEffects) extends Constraint(Pure) {}

case object PureHandler extends Handler[ElabOps, Pure.type](Pure) {
  override def run(c: Pure)(using ElabOps, SolverOps): Result = {
    Result.Done
  }
}