package chester.utils.elab

import chester.utils.cell.Cell


final class ProceduralCellId[T](
                           initialValue: Cell[T]
                         ) extends CellReprOf[T] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}


final class ProceduralSolver[Ops] (val conf: HandlerConf[Ops])(using Ops) extends SolverOps {

  override def readStable[U](id: CellReprOf[U]): Option[U] = ???

  override def readUnstable[U](id: CellReprOf[U]): Option[U] = ???

  override def run(): Unit = ???

  override def stable: Boolean = ???

  override def addConstraint(x: Constraint): Unit = ???

  override def fill[T](id: CellReprOf[T], value: T): Unit = ???

  override def hasStableValue(id: CellReprOfAny): Boolean = ???

  override def noStableValue(id: CellReprOfAny): Boolean = ???

  override def hasSomeValue(id: CellReprOfAny): Boolean = ???

  override def noAnyValue(id: CellReprOfAny): Boolean = ???
}
