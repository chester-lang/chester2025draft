package chester.utils.elab

import chester.utils.cell.{CellR, Cell}

trait SolverOps {
  def hasStableValue(id: CellReprOfAny): Boolean
  def noStableValue(id: CellReprOfAny): Boolean
  def readStable[U](id: CellReprOfR[U]): Option[U]
  def hasSomeValue(id: CellReprOfAny): Boolean
  def noAnyValue(id: CellReprOfAny): Boolean
  def readUnstable[U](id: CellReprOfR[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def addCell[A,B,C <: Cell[A,B]](cell: C): CellRepr[A,B,C] = ???
  def fill[T](id: CellReprOfW[T], value: T): Unit
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: CellReprOfR[T]): CellR[T]
  protected def updateCell[A,B](id: CellReprOf[A,B], f: Cell[A,B] => Cell[A,B]): Unit

  override def hasStableValue(id: CellReprOfAny): Boolean = peakCell(id).hasStableValue

  override def noStableValue(id: CellReprOfAny): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: CellReprOfR[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue(id: CellReprOfAny): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue(id: CellReprOfAny): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: CellReprOfR[U]): Option[U] = peakCell(id).readUnstable

  override def fill[T](id: CellReprOfW[T], value: T): Unit = updateCell(id, _.fill(value))
}

trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}
