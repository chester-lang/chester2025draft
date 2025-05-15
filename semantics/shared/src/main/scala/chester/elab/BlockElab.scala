package chester.elab

import chester.cell.CellEffects
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object BlockElab extends Kind {
  type Of = BlockElab
}

case class BlockElab(block: Block, ty: CellRWOr[Term])(using effects: CellEffects, elab: Elab, ops: SolverOps, ctx: Context)
    extends Constraint(BlockElab)
    with ConstraintTerm {
  override def result: CellRW[BlockTerm] = newHole
  given Context = ctx
  given Elab = elab
  given CellEffects = effects
}

case object BlockElabHandler extends Handler[ElabOps, BlockElab.type](BlockElab) {
  override def run(c: BlockElab)(using ElabOps, SolverOps): Result =
    ???

  override def defaulting(c: BlockElab, level: DefaultingLevel)(using ElabOps, SolverOps): Unit =
    ()
}
