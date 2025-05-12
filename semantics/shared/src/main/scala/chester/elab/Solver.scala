package chester.elab

import chester.uniqid.Uniqid

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
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

class CellId[T](
    val uniqId: Uniqid,
    initialValue: Cell[T]
) {
  val storeRef = new AtomicReference[Cell[T]](initialValue)
}

private trait BasicSolverOps extends SolverOps {

  override def hasStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasStableValue

  override def noStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().noStableValue

  override def readStable[U](id: CellId[U]): Option[U] = id.storeRef.get().readStable

  override def hasSomeValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasSomeValue

  override def noAnyValue[T](id: CellId[T]): Boolean = id.storeRef.get().noAnyValue

  override def readUnstable[U](id: CellId[U]): Option[U] = id.storeRef.get().readUnstable

}

case class WaitingConstraint(vars: Vector[CellId[?]], x: Constraint)

final class ConcurrentSolver[Ops] private (val conf: HandlerConf) extends BasicSolverOps {
  private val pool = new ForkJoinPool()
  private val delayedConstraints = new AtomicReference(Vector[WaitingConstraint]())
  private val failedConstraints = new AtomicReference(Vector[Constraint]())
  // implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  override def stable: Boolean = {
    if (delayedConstraints.get().nonEmpty) return false
    if (pool.isShutdown) return true
    if (pool.isQuiescent) {
      val tasks = pool.shutdownNow()
      assume(tasks.isEmpty)
      return true
    }
    return false
  }

  override def addConstraint(x: Constraint): Unit =
    pool.execute { () =>
      val handler = conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler"))
      val result = handler.run(x.asInstanceOf[handler.kind.ConstraintType])
      result match {
        case Result.Done => ()
        case Result.Failed =>
          val _ = failedConstraints.getAndUpdate(_.appended(x))
        case Result.Waiting(vars) =>
          val delayed = WaitingConstraint(vars, x)
          val _ = delayedConstraints.getAndUpdate(_.appended(delayed))
      }
    }

  @tailrec
  override def fill[T](id: CellId[T], value: T): Unit = {
    val current = id.storeRef.get()
    val read = current.readStable
    if (read.isDefined) {
      if (value == read.get) return
      throw new IllegalStateException("cannot overwrite stable value")
    }
    if(!id.storeRef.compareAndSet(current, current.fill(value))) {
      return fill(id, value)
    }
    // TODO: trigger some delayed contrains
  }
}

final class SinglethreadSolver[Ops] {}

trait SolverOps {
  def hasStableValue[T](id: CellId[T]): Boolean
  def noStableValue[T](id: CellId[T]): Boolean
  def readStable[U](id: CellId[U]): Option[U]
  def hasSomeValue[T](id: CellId[T]): Boolean
  def noAnyValue[T](id: CellId[T]): Boolean
  def readUnstable[U](id: CellId[U]): Option[U]

  def stable: Boolean

  def addConstraint(x: Constraint): Unit

  def fill[T](id: CellId[T], value: T): Unit
}
