package chester.utils.elab

open trait CellId[T] {
  def tag: String = Integer.toHexString(hashCode)

  final override def toString: String = s"CellId@${tag}" 
}

// Note that commit is equal or lower than actual commit
case class WaitingConstraint(vars: Vector[CellId[?]], x: Constraint) {
  def related(x: CellId[?]): Boolean = vars.contains(x)
}

trait SolverOps {
  def hasStableValue[T](id: CellId[T]): Boolean
  def noStableValue[T](id: CellId[T]): Boolean
  def readStable[U](id: CellId[U]): Option[U]
  def hasSomeValue[T](id: CellId[T]): Boolean
  def noAnyValue[T](id: CellId[T]): Boolean
  def readUnstable[U](id: CellId[U]): Option[U]

  def stable: Boolean

  def addConstraint(x: Constraint): Unit
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def fill[T](id: CellId[T], value: T): Unit
}


trait SolverFactory {
  def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps
}