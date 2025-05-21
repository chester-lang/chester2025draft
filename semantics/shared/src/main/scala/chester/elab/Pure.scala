package chester.elab

import chester.utils.elab.*

case object Pure extends Kind {
  type Of = Pure
}

case class Pure(effects: Eff) extends Constraint(Pure) {}

case object PureHandler extends Handler[ElabOps, Pure.type](Pure) {
  override def run(c: Pure)(using ElabOps, SolverOps): Result =
    Result.Done

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
