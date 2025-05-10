package chester.elab

import chester.uniqid.Uniqid

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

trait SolverConf {
  def getHandler(kind: Kind): Option[Handler]
}

class MutSolverConf extends SolverConf {
  val store = new mutable.HashMap[Kind, Handler]()

  override def getHandler(kind: Kind): Option[Handler] = store.get(kind)
}


class HoldCell[+T <: Cell[?]](
                               val uniqId: Uniqid,
                               initialValue: T
                             ) {
  val storeRef = new AtomicReference[Cell[?]](initialValue)
}
type CellId[T] = HoldCell[Cell[T]]

class Solver(val conf: MutSolverConf) {

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