package chester.utils.propagator

import chester.uniqid.{Uniqid, UniqidOf}

import java.util.concurrent.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

trait ProvideMultithread extends ProvideImpl {

  class HoldCell[+T <: Cell[?]](
      val uniqId: UniqidOf[Impl[?]],
      initialValue: T
  ) {
    private val storeRef = new AtomicReference[Cell[?]](initialValue)
    val readingPropagators = new ConcurrentLinkedQueue[PIdOf[Propagator[?]]]()
    val zonkingPropagators = new ConcurrentLinkedQueue[PIdOf[Propagator[?]]]()
    private val didChangeRef = new AtomicBoolean(false)

    def store: T = storeRef.get().asInstanceOf[T]

    /** Atomically updates the store */
    def compareAndSetStore(
        expectedValue: Cell[?],
        newValue: Cell[?]
    ): Boolean = {
      val result = storeRef.compareAndSet(expectedValue, newValue)
      if (result) {
        didChangeRef.set(true)
      }
      result
    }

    def didChange: Boolean = didChangeRef.get()
    def clearDidChange(): Boolean = didChangeRef.getAndSet(false)

    def noAnyValue: Boolean = store.noAnyValue
    def noStableValue: Boolean = store.noStableValue
  }

  type CIdOf[+T <: Cell[?]] = HoldCell[T]
  type PIdOf[+T <: Propagator[?]] = HoldPropagator[T]
  type CellId[T] = CIdOf[Cell[T]]
  type SeqId[T] = CIdOf[SeqCell[T]]

  def isCId(x: Any): Boolean = x.isInstanceOf[HoldCell[?]]

  override def assumeCId(x: Any): CIdOf[Cell[?]] =
    x.asInstanceOf[CIdOf[Cell[?]]]

  class HoldPropagator[+T <: Propagator[?]](
      val uniqId: UniqidOf[Impl[?]],
      initialValue: T
  ) {
    private val storeRef = new AtomicReference[Propagator[?]](initialValue)
    private val aliveRef = new AtomicBoolean(true)

    def store: T = storeRef.get().asInstanceOf[T]

    /** Atomically updates the store */
    def compareAndSetStore(
        expectedValue: Propagator[?],
        newValue: Propagator[?]
    ): Boolean =
      storeRef.compareAndSet(expectedValue, newValue)

    def alive: Boolean = aliveRef.get()

    def setAlive(value: Boolean): Unit =
      aliveRef.set(value)
  }

  override def stateAbilityImpl[Ability]: StateAbility[Ability] =
    Impl[Ability]()

  class Impl[Ability](
      val uniqId: UniqidOf[Impl[Ability]] = Uniqid.generate[Impl[Ability]]
  ) extends StateAbility[Ability] {

    private val propagators =
      new ConcurrentLinkedQueue[PIdOf[Propagator[Ability]]]()
    private val forkJoinPool = new ForkJoinPool(
      Runtime.getRuntime.availableProcessors()
    )

    // Thread-safe change tracking queue
    private val didChanged = new ConcurrentLinkedQueue[CIdOf[?]]()

    // Progress tracking flag
    private val didSomethingRef = new AtomicBoolean(false)

    def didSomething: Boolean = didSomethingRef.get()
    def setDidSomething(value: Boolean): Unit =
      didSomethingRef.set(value)

    // Thread-local for recursion depth tracking
    private val recursionDepthThreadLocal = new ThreadLocal[Integer] {
      override def initialValue(): Integer = 0
    }

    private def getRecursionDepth(): Int = {
      val depth = recursionDepthThreadLocal.get()
      if (depth == null) 0 else depth.intValue()
    }

    private def setRecursionDepth(depth: Int): Unit =
      recursionDepthThreadLocal.set(depth)

    private def incrementRecursionDepth(): Int = {
      val current = getRecursionDepth()
      setRecursionDepth(current + 1)
      current + 1
    }

    private def decrementRecursionDepth(): Int = {
      val current = getRecursionDepth()
      if (current > 0) {
        setRecursionDepth(current - 1)
        current - 1
      } else {
        0
      }
    }

    override def readCell[T <: Cell[?]](id: CIdOf[T]): Option[T] = {
      require(id.uniqId == uniqId)
      Some(id.store)
    }

    override def update[T <: Cell[?]](id: CIdOf[T], f: T => T)(using
        Ability
    ): Unit = {
      require(id.uniqId == uniqId)
      var updated = false
      while (!updated) {
        val oldValue = id.store
        val newValue = f(oldValue)
        updated = id.compareAndSetStore(oldValue, newValue)
        if (updated) {
          // Track that something changed
          setDidSomething(true)
          // Add to the changed queue
          didChanged.add(id)
          // Immediately process dependent propagators
          processPropagators(id)
        }
      }
    }

    override def tick(using Ability): Unit = {
      // Process all changed cells
      var cell = didChanged.poll()
      while (cell != null) {
        if (cell.clearDidChange()) {
          processPropagators(cell)
        }
        cell = didChanged.poll()
      }
    }

    override def fill[T <: Cell[U], U](id: CIdOf[T], value: U)(using
        Ability
    ): Unit = {
      require(id.uniqId == uniqId)
      var updated = false
      while (!updated) {
        val oldValue = id.store
        val newValue = oldValue.fill(value).asInstanceOf[T]
        updated = id.compareAndSetStore(oldValue, newValue)
        if (updated) {
          // Track that something changed
          setDidSomething(true)
          // Add to the changed queue
          didChanged.add(id)
          // Immediately process dependent propagators
          processPropagators(id)
        }
      }
    }

    override def addCell[T <: Cell[?]](cell: T): CIdOf[T] =
      new HoldCell[T](uniqId, cell)

    override def addPropagatorGetPid[T <: Propagator[Ability]](
        propagator: T
    )(using Ability): PIdOf[T] = {
      val id = new HoldPropagator[T](uniqId, propagator)
      propagators.add(id.asInstanceOf[PIdOf[Propagator[Ability]]])

      for (cell <- propagator.zonkingCells)
        cell.zonkingPropagators.add(id.asInstanceOf[PIdOf[Propagator[?]]])
      for (cell <- propagator.readingCells)
        cell.readingPropagators.add(id.asInstanceOf[PIdOf[Propagator[?]]])

      // Submit task to process this propagator
      forkJoinPool.execute(
        new PropagatorTask(
          Vector(id.asInstanceOf[HoldPropagator[Propagator[Ability]]]),
          this
        )
      )
      id
    }

    override def stable: Boolean = didChanged.isEmpty

    // Process propagators in batches to improve performance
    private def processPropagators(
        changedCell: CIdOf[?]
    )(using Ability): Unit = {
      val dependentPropagators = changedCell.readingPropagators
        .iterator()
        .asScala
        .filter(_.alive)
        .toVector
      if (dependentPropagators.nonEmpty) {
        forkJoinPool.execute(
          new BatchPropagatorTask(
            dependentPropagators
              .asInstanceOf[Vector[HoldPropagator[Propagator[Ability]]]],
            this
          )
        )
      }
    }

    override def naiveZonk(
        cells: Vector[CIdOf[Cell[?]]]
    )(using Ability): Unit = {
      val currentDepth = incrementRecursionDepth()
      try {
        // Skip if recursion is too deep
        if (currentDepth > 5) {
          setDidSomething(true)
          return
        }

        // Skip duplicate cells to prevent redundant processing
        val uniqueCells = cells.distinct

        // Keep track of processed cells to detect cycles
        val processedCellIds = scala.collection.mutable.Set[String]()
        uniqueCells.foreach(c => processedCellIds.add(c.toString))

        var cellsNeeded = uniqueCells
        var tryFallback = 0
        var loop = true
        var iterationCount = 0

        while (loop && iterationCount < 10) {
          iterationCount += 1

          // Break if iterations are excessive
          if (iterationCount >= 10) {
            setDidSomething(true)
            // Instead of using break(), we exit the loop normally
            loop = false
            return
          }

          setDidSomething(false)

          // Process any remaining propagators
          tick

          cellsNeeded = cellsNeeded.filter(this.noAnyValue(_))

          if (cellsNeeded.isEmpty) {
            loop = false
          } else {
            // Process cells that still need values
            val tasks = cellsNeeded.flatMap { c =>
              if (c.noAnyValue) {
                // Limit to top propagators by score
                val alivePropagators = c.zonkingPropagators.asScala
                  .filter(_.alive)
                  .toVector
                  .sortBy(p => -p.store.score)

                // Limit number of propagators we try per cell
                val limitedPropagators = if (alivePropagators.size > 3) {
                  alivePropagators.take(3)
                } else {
                  alivePropagators
                }

                limitedPropagators.map { p =>
                  new ZonkTask(
                    c,
                    p.asInstanceOf[HoldPropagator[Propagator[Ability]]],
                    firstFallback = tryFallback == 0,
                    this,
                    currentDepth,
                    processedCellIds
                  )
                }
              } else {
                Vector.empty
              }
            }

            if (tasks.nonEmpty) {
              try {
                val _ = ForkJoinTask.invokeAll(tasks.asJava)
                tick
              } catch {
                case e: Exception =>
                  // Log exception but continue processing
                  setDidSomething(true)
              }
            }

            // Check if we made progress
            if (!didSomething) {
              tryFallback += 1
            } else {
              // Reset fallback counter if we made progress
              tryFallback = 0
              // Update cells list
              cellsNeeded = cellsNeeded.filter(_.noAnyValue)
              if (cellsNeeded.isEmpty) {
                loop = false
              }
            }
          }
        }

        // If still not resolved, try default values
        if (cellsNeeded.nonEmpty && tryFallback > 1) {
          val defaultTasks = cellsNeeded.flatMap { c =>
            Option.when(c.noAnyValue && c.store.default.isDefined)(new DefaultValueTask(c, this))
          }

          if (defaultTasks.nonEmpty) {
            try {
              val _ = ForkJoinTask.invokeAll(defaultTasks.asJava)
              // Final check for unresolved cells
              cellsNeeded = cellsNeeded.filter(_.noAnyValue)
            } catch {
              case e: Exception =>
                // Continue even if there's an exception
                setDidSomething(true)
            }
          }

          // Throw exception for cells that couldn't be resolved
          if (cellsNeeded.nonEmpty) {
            throw new IllegalStateException(
              s"Cells $cellsNeeded are not covered by any propagator"
            )
          }
        }
      } catch {
        case e: IllegalStateException =>
          // Rethrow illegal state exceptions
          throw e
        case e: Exception =>
          // Log other exceptions but allow processing to continue
          setDidSomething(true)
      } finally decrementRecursionDepth(): Unit
    }

    class BatchPropagatorTask(
        propagators: Vector[PIdOf[Propagator[Ability]]],
        state: Impl[Ability]
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit =
        try
          propagators.foreach { p =>
            require(p.uniqId == uniqId)
            if (p.alive) {
              val propagator = p.store
              val done = propagator.run(using state, more)
              if (done) {
                p.setAlive(false)
                val _ = state.propagators.remove(p)
                setDidSomething(true)
              }
            }
          }
        catch {
          case ex: Exception =>
            // Just mark as doing something and continue
            setDidSomething(true)
        }
    }

    class PropagatorTask(
        propagators: Vector[PIdOf[Propagator[Ability]]],
        state: Impl[Ability]
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit =
        try
          propagators.foreach { p =>
            require(p.uniqId == uniqId)
            if (p.alive) {
              val propagator = p.store
              val done = propagator.run(using state, more)
              if (done) {
                p.setAlive(false)
                val _ = state.propagators.remove(p)
                setDidSomething(true)
              }
            }
          }
        catch {
          case ex: Exception =>
            // Just mark as doing something and continue
            setDidSomething(true)
        }
    }

    class ZonkTask(
        c: CIdOf[Cell[?]],
        p: PIdOf[Propagator[Ability]],
        firstFallback: Boolean,
        state: Impl[Ability],
        parentRecursionDepth: Int = 0,
        processedCellIds: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit = {
        require(c.uniqId == uniqId)
        require(p.uniqId == uniqId)
        if (c.noAnyValue && p.alive) {
          // Skip if recursion is too deep
          if (parentRecursionDepth > 3) {
            setDidSomething(true)
            return
          }

          val propagator = p.store
          val zonkResult =
            try
              if (firstFallback) {
                propagator.naiveZonk(Vector(c))(using state, more)
              } else {
                propagator.naiveFallbackZonk(Vector(c))(using state, more)
              }
            catch {
              case e: Exception =>
                // Continue with NotYet result on exception
                ZonkResult.NotYet
            }

          zonkResult match {
            case ZonkResult.Done =>
              p.setAlive(false)
              val _ = propagators.remove(p)
              setDidSomething(true)
            case ZonkResult.Require(needed) =>
              // Filter out cells we've already seen to prevent cycles
              val newNeeded = needed.toVector
                .filter(n => n.noStableValue)
                .filterNot(n => n == c)
                .filterNot(n => processedCellIds.contains(n.toString))

              if (newNeeded.nonEmpty) {
                // Mark these cells as processed before recursing
                newNeeded.foreach(n => processedCellIds.add(n.toString))
                // Process the newly needed cells
                try {
                  state.naiveZonk(newNeeded)
                  setDidSomething(true)
                } catch {
                  case e: Exception =>
                    // Just mark as doing something and continue
                    setDidSomething(true)
                }
              }
            case ZonkResult.NotYet =>
            // No action needed
          }
        }
      }
    }

    class DefaultValueTask(c: CIdOf[Cell[?]], state: Impl[Ability])(using
        Ability
    ) extends RecursiveAction {
      override def compute(): Unit =
        if (c.noAnyValue && c.store.default.isDefined) {
          state.fill(c.asInstanceOf[CIdOf[Cell[Any]]], c.store.default.get)
          setDidSomething(true)
        }
    }
  }
}
