package chester.utils.elab

import chester.utils.cell.{Cell, CellRW}

sealed trait CellIdAny {

}

open trait CellRepr[+A,-B, +C <: CellRW[A,B]] extends CellIdAny {
  def tag: String = Integer.toHexString(hashCode)

  final override def toString: String = s"CellId@${tag}" 
}

type CellReprOf[T] = CellRepr[T, T, CellRW[T,T]]

// Note that the commit is equal or lower than the actual commit
case class WaitingConstraint(vars: Vector[CellIdAny], x: Constraint) {
  def related(x: CellIdAny): Boolean = vars.contains(x)
}

trait SolverOps {
  def hasStableValue[T](id: CellReprOf[T]): Boolean
  def noStableValue[T](id: CellReprOf[T]): Boolean
  def readStable[U](id: CellReprOf[U]): Option[U]
  def hasSomeValue[T](id: CellReprOf[T]): Boolean
  def noAnyValue[T](id: CellReprOf[T]): Boolean
  def readUnstable[U](id: CellReprOf[U]): Option[U]

  def run(): Unit
  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def fill[T](id: CellReprOf[T], value: T): Unit
}

trait BasicSolverOps extends SolverOps {
  protected def peakCell[T](id: CellReprOf[T]): Cell[T]
  protected def updateCell[T](id: CellReprOf[T], f: Cell[T]=>Cell[T]):Unit


  override def hasStableValue[T](id: CellReprOf[T]): Boolean = peakCell(id).hasStableValue

  override def noStableValue[T](id: CellReprOf[T]): Boolean = peakCell(id).noStableValue

  override def readStable[U](id: CellReprOf[U]): Option[U] = peakCell(id).readStable

  override def hasSomeValue[T](id: CellReprOf[T]): Boolean = peakCell(id).hasSomeValue

  override def noAnyValue[T](id: CellReprOf[T]): Boolean = peakCell(id).noAnyValue

  override def readUnstable[U](id: CellReprOf[U]): Option[U] = peakCell(id).readUnstable


  override def fill[T](id: CellReprOf[T], value: T): Unit = updateCell(id, _.fill(value))
}


trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}