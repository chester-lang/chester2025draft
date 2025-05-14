package chester.utils.elab

import chester.utils.cell.*

import java.util.concurrent.{ForkJoinPool, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.boundary

final class ConcurrentCell[+A, -B, C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  val storeRef = new AtomicReference[C](initialValue)
}

object ConcurrentSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ConcurrentSolver(conf)
}

final class ConcurrentSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {

  implicit inline def thereAreAllConcurrent[A, B, C <: CellContent[A, B]](inline x: Cell[A, B, C]): ConcurrentCell[A, B, C] =
    x.asInstanceOf[ConcurrentCell[A, B, C]]

  override protected def peakCell[T](id: ReprR[T]): CellR[T] = id.storeRef.get()

  given SolverOps = this
  private val pool = new ForkJoinPool()
  private val delayedConstraints = new AtomicReference(Vector[WaitingConstraint]())
  private val failedConstraints = new AtomicReference(Vector[Constraint]())
  // implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)

  private def entropy() = delayedConstraints.get().map(c => c.x.kind.hashCode() << 8 + c.x.hashCode()).sorted.toVector

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
  private def inPoolTickStage1(zonkLevel: DefaultingLevel): Unit = {
    val delayed = delayedConstraints.getAndSet(Vector.empty)
    delayed.foreach(x => doDefaulting(x.x, zonkLevel))
  }
  override def run(): Unit = boundary[Unit] { outer ?=>
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
      for (level <- DefaultingLevel.Values) {
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

  private def doDefaulting(x: Constraint, zonkLevel: DefaultingLevel): Unit =
    pool.execute { () =>
      val handler = conf.getHandler(x.kind).getOrElse(throw new IllegalStateException("no handler"))
      handler.defaulting(x.asInstanceOf[handler.kind.ConstraintType], zonkLevel)
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
  override protected def updateCell[A, B](id: Repr[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    val current = id.storeRef.get()
    val updated = f(current)
    if (current == updated) {
      return
    }
    if (!id.storeRef.compareAndSet(current, updated)) {
      return updateCell(id, f)
    }
    // Note that here is a possible race condition that delayed constraints might haven't been added to delayedConstraints
    val prev = delayedConstraints.getAndUpdate(_.filterNot(_.related(id)))
    val related = prev.withFilter(_.related(id)).map(_.x)
    addConstraints(related)
  }
}
