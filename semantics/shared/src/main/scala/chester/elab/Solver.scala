package chester.elab

import chester.uniqid.Uniqid

import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.TrieMap

trait HandlerConf {
  def getHandler(kind: Kind): Option[Handler]
}

final class MutHandlerConf(hs: Handler*) extends HandlerConf {
  private val store = TrieMap[Kind, Handler](hs.map(h => (h.kind, h))*)

  override def getHandler(kind: Kind): Option[Handler] = store.get(kind)

  def register(handler: Handler): Unit = {
    val oldValue = store.putIfAbsent(handler.kind, handler)
    if (oldValue.isDefined) throw new IllegalStateException("already")
  }
}

val DefaultSolverConf = new MutHandlerConf(MergeSimpleHandler)

class CellHere[T](
    val uniqId: Uniqid,
    initialValue: Cell[T]
) {
  val storeRef = new AtomicReference[Cell[T]](initialValue)
}
opaque type CellId[T] = CellHere[T]

private implicit inline def notOpaque[T](inline x: CellId[T]): CellHere[T] = x.asInstanceOf[CellHere[T]]

private trait BasicSolverOps extends SolverOps {

  override def hasStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasStableValue

  override def noStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().noStableValue

  override def readStable[U](id: CellId[U]): Option[U] = id.storeRef.get().readStable

  override def hasSomeValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasSomeValue

  override def noAnyValue[T](id: CellId[T]): Boolean = id.storeRef.get().noAnyValue

  override def readUnstable[U](id: CellId[U]): Option[U] = id.storeRef.get().readUnstable

}

final class ConcurrentSolver[Ops](val conf: HandlerConf) extends BasicSolverOps {

  override def stable: Boolean = ???

  override def addConstraint(x: Constraint): Unit = ???
}

final class SinglethreadSolver[Ops] {

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
}
