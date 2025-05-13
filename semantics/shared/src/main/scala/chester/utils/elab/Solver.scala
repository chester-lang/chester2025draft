package chester.utils.elab

import chester.utils.cell.{Cell, CellR, CellRW}

sealed trait CellIdAny {}

open trait CellRepr[+A, -B, +C <: CellRW[A, B]] extends CellIdAny {
  def tag: String = Integer.toHexString(hashCode)

  override final def toString: String = s"CellId@$tag"
}

type CellReprOf[T] = CellRepr[T, T, CellRW[T, T]]
type CellReprOfAny = CellRepr[Any, Nothing, CellRW[Any, Nothing]]
type CellReprOfR[+T] = CellRepr[T, Nothing, CellRW[T, Nothing]]

// Note that the commit is equal or lower than the actual commit
case class WaitingConstraint(vars: Vector[CellIdAny], x: Constraint) {
  def related(x: CellIdAny): Boolean = vars.contains(x)
}

trait SolverOps {
  def hasStableValue(id: CellReprOfAny): Boolean
  def noStableValue(id: CellReprOfAny): Boolean
  def readStable[U](id: CellReprOf[U]): Option[U]
  def hasSomeValue(id: CellReprOfAny): Boolean
  def noAnyValue(id: CellReprOfAny): Boolean
  def readUnstable[U](id: CellReprOf[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def fill[T](id: CellReprOf[T], value: T): Unit
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: CellReprOfR[T]): CellR[T]
  protected def updateCell[T](id: CellReprOf[T], f: Cell[T] => Cell[T]): Unit

  override def hasStableValue(id: CellReprOfAny): Boolean = peakCell(id).hasStableValue

  override def noStableValue(id: CellReprOfAny): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: CellReprOf[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue(id: CellReprOfAny): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue(id: CellReprOfAny): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: CellReprOf[U]): Option[U] = peakCell(id).readUnstable

  override def fill[T](id: CellReprOf[T], value: T): Unit = updateCell(id, _.fill(value))
}

trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}
