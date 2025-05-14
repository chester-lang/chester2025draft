package chester.elab

import chester.cell.CellEffects
import chester.utils.elab.{Constraint, Kind}

case object Pure extends Kind {
  type Of = Pure
}

case class Pure(effects: CellEffects) extends Constraint(Pure) {}
