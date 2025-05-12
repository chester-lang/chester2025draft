package chester.elab

import chester.uniqid.Uniqid

import java.util.concurrent.ForkJoinPool
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

final class ConcurrentSolver[Ops] private (val conf: HandlerConf) extends BasicSolverOps {
  private val pool = new ForkJoinPool()
  private val delayedConstraints = new AtomicReference(Vector[Constraint]())
  //implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  override def stable: Boolean = {
    if(delayedConstraints.get().nonEmpty) return false
    if(pool.isShutdown) return true
    if(pool.isQuiescent) {
      val tasks = pool.shutdownNow()
      assume(tasks.isEmpty)
      return true
    }
    return false
  }

  override def addConstraint(x: Constraint): Unit = {
    pool.execute(() => {
      val handler = conf.getHandler(x.kind).getOrElse{throw new IllegalStateException("no handler")}
      val result = handler.run(x.asInstanceOf[handler.kind.ConstraintType])
      // TODO: more logic
    })
  }
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
