package chester.utils.elab

import chester.utils.cell.Cell


final class ProceduralCellId[T](
                           initialValue: Cell[T]
                         ) extends CellId[T] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}


final class ProceduralSolver[Ops] (val conf: HandlerConf[Ops])(using Ops) extends SolverOps {

  override def hasStableValue[T](id: CellId[T]): Boolean = ???

  override def noStableValue[T](id: CellId[T]): Boolean = ???

  override def readStable[U](id: CellId[U]): Option[U] = ???

  override def hasSomeValue[T](id: CellId[T]): Boolean = ???

  override def noAnyValue[T](id: CellId[T]): Boolean = ???

  override def readUnstable[U](id: CellId[U]): Option[U] = ???

  override def run(): Unit = ???

  override def stable: Boolean = ???

  override def addConstraint(x: Constraint): Unit = ???

  override def fill[T](id: CellId[T], value: T): Unit = ???
}
