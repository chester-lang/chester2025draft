package chester.elab

import chester.utils.elab.{Constraint, Kind}

case object Pure extends Kind {
  type Of = Pure
}

case class Pure() extends Constraint(Pure) {
  
}