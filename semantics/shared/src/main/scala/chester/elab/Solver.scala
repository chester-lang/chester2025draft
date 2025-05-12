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

// Note that commit is equal or lower than actual commit
case class WaitingConstraint(vars: Vector[CellId[?]], x: Constraint) {
  def related(x: CellId[?]): Boolean = vars.contains(x)
}

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

  // normally run when quiescent and need to be safe run at any time
  private def inPoolTickStage0(): Unit = {
    // in case of race condition
    val resubmitDelayed = delayedConstraints.getAndSet(Vector.empty)
    addConstraints(resubmitDelayed.map(_.x))
  }
  private def inPoolTickStage1(): Unit = {
    val delayed = delayedConstraints.getAndSet(Vector.empty)
    
  }

  private def  doZonk(x: Constraint): Unit =
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
    val prev = delayedConstraints.getAndUpdate(_.filterNot(_.related(id)))
    val related = prev.filter(_.related(id)).map(_.x)
    addConstraints(related)
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
  def addConstraints(xs: Seq[Constraint]): Unit = xs.foreach(addConstraint)

  def fill[T](id: CellId[T], value: T): Unit
}
