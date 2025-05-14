package chester.utils.elab

import chester.utils.cell.{CellContent, CellR, CellRW}

final class ProceduralCell[T](
    initialValue: CellRW[T]
) extends ReprRW[T] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}

final class ProceduralSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {

  override protected def peakCell[T](id: ReprR[T]): CellR[T] = ???

  override protected def updateCell[A, B](id: Repr[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = ???

  override def run(): Unit = ???

  override def stable: Boolean = ???

  override def addConstraint(x: Constraint): Unit = ???
}
