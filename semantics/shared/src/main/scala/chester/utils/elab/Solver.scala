package chester.utils.elab

import chester.utils.cell.{CellContent, CellR}

trait SolverOps {
  def hasStableValue(id: ReprAny): Boolean
  def noStableValue(id: ReprAny): Boolean
  def readStable[U](id: ReprR[U]): Option[U]
  def hasSomeValue(id: ReprAny): Boolean
  def noAnyValue(id: ReprAny): Boolean
  def readUnstable[U](id: ReprR[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ???
  def fill[T](id: ReprW[T], value: T): Unit
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: ReprR[T]): CellR[T]
  protected def updateCell[A, B](id: Repr[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit

  override def hasStableValue(id: ReprAny): Boolean = peakCell(id).hasStableValue

  override def noStableValue(id: ReprAny): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: ReprR[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue(id: ReprAny): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue(id: ReprAny): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: ReprR[U]): Option[U] = peakCell(id).readUnstable

  override def fill[T](id: ReprW[T], value: T): Unit = updateCell(id, _.fill(value))
}

trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}
