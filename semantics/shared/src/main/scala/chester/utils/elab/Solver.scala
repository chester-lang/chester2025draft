package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR}

trait SolverOps {
  def hasStableValue(id: CellAny): Boolean
  def noStableValue(id: CellAny): Boolean
  def readStable[U](id: CellR[U]): Option[U]
  def hasSomeValue(id: CellAny): Boolean
  def noAnyValue(id: CellAny): Boolean
  def readUnstable[U](id: CellR[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ???
  def fill[T](id: CellW[T], value: T): Unit
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: CellR[T]): CellContentR[T]
  protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit

  override def hasStableValue(id: CellAny): Boolean = peakCell(id).hasStableValue

  override def noStableValue(id: CellAny): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: CellR[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue(id: CellAny): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue(id: CellAny): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: CellR[U]): Option[U] = peakCell(id).readUnstable

  override def fill[T](id: CellW[T], value: T): Unit = updateCell(id, _.fill(value))
}

trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}
