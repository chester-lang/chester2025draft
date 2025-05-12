package chester.elab

import chester.uniqid.Uniqid

import java.util.concurrent.atomic.AtomicReference
import scala.collection.concurrent.TrieMap

trait SolverConf {
  def getHandler(kind: Kind): Option[Handler]
}

final class MutSolverConf(hs: Handler*) extends SolverConf {
  private val store = TrieMap[Kind, Handler](hs.map(h => (h.kind, h))*)

  override def getHandler(kind: Kind): Option[Handler] = store.get(kind)

  def register(handler: Handler): Unit = {
    val oldValue = store.putIfAbsent(handler.kind, handler)
    if (oldValue.isDefined) throw new IllegalStateException("already")
  }
}

val DefaultSolverConf = new MutSolverConf(MergeSimpleHandler)

class HoldCell[+T <: Cell[?]](
    val uniqId: Uniqid,
    initialValue: T
) {
  val storeRef = new AtomicReference[Cell[?]](initialValue)
}
type CellId[T] = HoldCell[Cell[T]]

trait Solver {

}

class DefaultSolver(val conf: MutSolverConf) extends Solver {
  var constrains: Vector[Constrain] = Vector()

}

trait SolverState {
  def hasStableValue[T](id: CellId[T]): Boolean
  def noStableValue[T](id: CellId[T]): Boolean
  def readStable[U](id: CellId[U]): Option[U]
  def hasUnstableValue[T](id: CellId[T]): Boolean
  def noUnstableValue[T](id: CellId[T]): Boolean
  def readUnstable[U](id: CellId[U]): Option[U]

  def stable: Boolean
}
