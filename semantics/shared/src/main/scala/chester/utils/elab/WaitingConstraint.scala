package chester.utils.elab

case class WaitingConstraint(vars: Vector[CellReprAny], x: Constraint) {
  def related(x: CellReprAny): Boolean = vars.contains(x)
}
