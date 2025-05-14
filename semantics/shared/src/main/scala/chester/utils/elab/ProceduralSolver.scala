package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR, CellContentRW}

final class ProceduralCell[T](
    initialValue: CellContentRW[T]
) extends CellRW[T] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}

final class ProceduralSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = ???

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = ???

  override def run(): Unit = ???

  override def stable: Boolean = ???

  override def addConstraint(x: Constraint): Unit = ???
}
