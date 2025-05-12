package chester.utils.elab


import chester.uniqid.Uniqid

import java.util.concurrent.{ForkJoinPool, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.boundary

class ConcurrentCellId[T](
                           val uniqId: Uniqid,
                           initialValue: Cell[T]
                         ) extends CellId[T] {
  val storeRef = new AtomicReference[Cell[T]](initialValue)
}

final class ConcurrentSolver[Ops] private (val conf: HandlerConf[Ops])(using Ops) extends SolverOps {

  implicit inline def thereAreAllConcurrent[T](inline x: CellId[T]): ConcurrentCellId[T] = x.asInstanceOf[ConcurrentCellId[T]]
  override def hasStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasStableValue

  override def noStableValue[T](id: CellId[T]): Boolean = id.storeRef.get().noStableValue

  override def readStable[U](id: CellId[U]): Option[U] = id.storeRef.get().readStable

  override def hasSomeValue[T](id: CellId[T]): Boolean = id.storeRef.get().hasSomeValue

  override def noAnyValue[T](id: CellId[T]): Boolean = id.storeRef.get().noAnyValue

  override def readUnstable[U](id: CellId[U]): Option[U] = id.storeRef.get().readUnstable

  given SolverOps = this
  private val pool = new ForkJoinPool()
  private val delayedConstraints = new AtomicReference(Vector[WaitingConstraint]())
  private val failedConstraints = new AtomicReference(Vector[Constraint]())
  // implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  private def entropy() = delayedConstraints.get().map(c=>c.x.kind.hashCode()<<8+c.x.hashCode()).sorted.toVector

  override def stable: Boolean = {
    if (delayedConstraints.get().nonEmpty) return false
    if (pool.isShutdown) return true
    if (pool.isQuiescent) {
      finish()
      return true
    }
    return false
  }

  private def finish(): Unit = {
    assume(pool.isQuiescent)
    assume(delayedConstraints.get().isEmpty)
    assume(pool.isQuiescent)
    assume(delayedConstraints.get().isEmpty)
    val tasks = pool.shutdownNow()
    assume(tasks.isEmpty)
  }

  // normally run when quiescent and need to be safe run at any time
  private def inPoolTickStage0(): Unit = {
    // in case of race condition
    val resubmitDelayed = delayedConstraints.getAndSet(Vector.empty)
    addConstraints(resubmitDelayed.map(_.x))
  }
  private def inPoolTickStage1(zonkLevel: ZonkLevel): Unit = {
    val delayed = delayedConstraints.getAndSet(Vector.empty)
    delayed.foreach(x => doZonk(x.x, zonkLevel))
  }
  def run(): Unit = boundary[Unit] { outer ?=>
    while (true) boundary[Unit] { inner ?=>
      assume(!pool.isShutdown)
      assume(pool.isQuiescent)
      pool.execute(() => inPoolTickStage0())
      val _ = pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
      assume(pool.isQuiescent)
      if (delayedConstraints.get().isEmpty) {
        finish()
        boundary.break()(using outer)
      }
      for (level <- ZonkLevel.Values) {
        val entropyBefore = entropy()
        assume(!pool.isShutdown)
        assume(pool.isQuiescent)
        pool.execute(() => inPoolTickStage1(level))
        val _ = pool.awaitQuiescence(Long.MaxValue, TimeUnit.DAYS)
        assume(pool.isQuiescent)
        if (delayedConstraints.get().isEmpty) {
          finish()
          boundary.break()(using outer)
        }
        if (entropy() != entropyBefore) boundary.break()(using inner)
      }
      throw new IllegalStateException("cannot finish")
    }
  }

  private def doZonk(x: Constraint, zonkLevel: ZonkLevel): Unit =
    pool.execute { () =>
      val handler = conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler"))
      handler.zonk(x.asInstanceOf[handler.kind.ConstraintType], zonkLevel)
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
    if (!id.storeRef.compareAndSet(current, current.fill(value))) {
      return fill(id, value)
    }
    // Note that here is a possible race condition that delayed constraints might haven't been added to delayedConstraints
    val prev = delayedConstraints.getAndUpdate(_.filterNot(_.related(id)))
    val related = prev.filter(_.related(id)).map(_.x)
    addConstraints(related)
  }
}
