package chester.utils.propagator

import chester.uniqid.{Uniqid, UniqidOf}

import java.util.concurrent.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*
import scala.util.control.Breaks.*

trait ProvideMultithread extends ProvideImpl {

  // Debug flag for cell propagation
  private val DEBUG_MULTITHREAD = true
  
  // Add a maximum number of retry attempts for cell operations
  private val MAX_RETRIES = 10
  
  // Add a timeout mechanism for propagation tasks
  private val PROPAGATION_TIMEOUT_MS = 1000 // 1 second timeout for propagation tasks

  // Debug method that only prints when DEBUG_MULTITHREAD is true
  private def debugPrint(message: => String): Unit = {
    if (DEBUG_MULTITHREAD) println(s"[ProvideMultithread] $message")
  }

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
        debugPrint(s"Cell $this updated: $expectedValue -> $newValue")
      } else {
        debugPrint(s"Cell $this update failed: expected $expectedValue but got ${storeRef.get()}")
      }
      result
    }

    def didChange: Boolean = didChangeRef.get()
    def clearDidChange(): Boolean = didChangeRef.getAndSet(false)

    def noAnyValue: Boolean = {
      val hasNoValue = store.noAnyValue
      if (hasNoValue) {
        debugPrint(s"Cell $this has no value")
      }
      hasNoValue
    }
    
    def noStableValue: Boolean = {
      val hasNoStableValue = store.noStableValue
      if (hasNoStableValue) {
        debugPrint(s"Cell $this has no stable value")
      }
      hasNoStableValue
    }
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
    ): Boolean = {
      storeRef.compareAndSet(expectedValue, newValue)
    }

    def alive: Boolean = aliveRef.get()

    def setAlive(value: Boolean): Unit = {
      aliveRef.set(value)
    }
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
    def setDidSomething(value: Boolean): Unit = {
      didSomethingRef.set(value)
    }

    // Thread-local for recursion depth tracking
    private val recursionDepthThreadLocal = new ThreadLocal[Integer] {
      override def initialValue(): Integer = 0
    }

    private def getRecursionDepth(): Int = {
      val depth = recursionDepthThreadLocal.get()
      if (depth == null) 0 else depth.intValue()
    }

    private def setRecursionDepth(depth: Int): Unit = {
      recursionDepthThreadLocal.set(depth)
    }

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
      val result = Some(id.store)
      debugPrint(s"readCell($id) => ${result.map(_.getClass.getSimpleName)}")
      result
    }

    override def readUnstable[U](id: CellId[U]): Option[U] = {
      val cellResult = readCell[Cell[U]](id)
      val result = cellResult.flatMap(_.readUnstable)
      debugPrint(s"readUnstable($id) => $result (cell present: ${cellResult.isDefined})")
      result
    }

    override def update[T <: Cell[?]](id: CIdOf[T], f: T => T)(using
        Ability
    ): Unit = {
      require(id.uniqId == uniqId)
      debugPrint(s"Updating cell $id")
      var updated = false
      var attempts = 0
      while (!updated && attempts < 10) {
        attempts += 1
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
          debugPrint(s"Successfully updated cell $id after $attempts attempts")
        } else if (attempts >= 10) {
          // Safety mechanism to prevent infinite loops
          debugPrint(s"WARNING: Failed to update cell $id after $attempts attempts - forcing update")
          // Force an update as a last resort
          val latestValue = id.store
          val finalValue = f(latestValue)
          if (id.compareAndSetStore(latestValue, finalValue)) {
            setDidSomething(true)
            didChanged.add(id)
            processPropagators(id)
            debugPrint(s"Forced update of cell $id succeeded")
          } else {
            debugPrint(s"ERROR: Even forced update of cell $id failed!")
          }
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
      debugPrint(s"Filling cell $id with value $value")
      var updated = false
      var attempts = 0
      while (!updated && attempts < 10) {
        attempts += 1
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
          debugPrint(s"Successfully filled cell $id after $attempts attempts")
        } else if (attempts >= 10) {
          // Safety mechanism to prevent infinite loops
          debugPrint(s"WARNING: Failed to fill cell $id after $attempts attempts")
        }
      }
    }

    override def addCell[T <: Cell[?]](cell: T): CIdOf[T] = {
      new HoldCell[T](uniqId, cell)
    }

    override def addPropagatorGetPid[T <: Propagator[Ability]](
        propagator: T
    )(using Ability): PIdOf[T] = {
      val id = new HoldPropagator[T](uniqId, propagator)
      propagators.add(id.asInstanceOf[PIdOf[Propagator[Ability]]])

      for (cell <- propagator.zonkingCells) {
        cell.zonkingPropagators.add(id.asInstanceOf[PIdOf[Propagator[?]]])
      }
      for (cell <- propagator.readingCells) {
        cell.readingPropagators.add(id.asInstanceOf[PIdOf[Propagator[?]]])
      }

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
      debugPrint(s"naiveZonk called with ${cells.size} cells at depth $currentDepth")
      try {
        // Skip if recursion is too deep
        if (currentDepth > 5) {
          debugPrint(s"Recursion depth $currentDepth exceeded limit - terminating naiveZonk")
          setDidSomething(true)
          return
        }

        // Safety check: empty cells vector
        if (cells.isEmpty) {
          debugPrint("naiveZonk called with empty cell vector, nothing to do")
          return
        }

        // Safety check: detect any null cells
        if (cells.exists(_ == null)) {
          debugPrint("WARNING: naiveZonk called with null cells, filtering them out")
          val nonNullCells = cells.filter(_ != null)
          if (nonNullCells.isEmpty) {
            debugPrint("All cells were null, nothing to do")
            return
          }
          naiveZonk(nonNullCells)
          return
        }

        // Skip duplicate cells to prevent redundant processing
        val uniqueCells = cells.distinct
        debugPrint(s"Processing ${uniqueCells.size} unique cells")

        // Keep track of processed cells to detect cycles
        val processedCellIds = scala.collection.mutable.Set[String]()
        uniqueCells.foreach(c => processedCellIds.add(c.toString))

        var cellsNeeded = uniqueCells
        var tryFallback = 0
        var loop = true
        var iterationCount = 0
        
        // Add a timeout mechanism
        val startTime = System.currentTimeMillis()

        breakable {
          while (loop && iterationCount < 10) {
            iterationCount += 1
            debugPrint(s"naiveZonk iteration $iterationCount, cells needed: ${cellsNeeded.size}")
            
            // Check for timeout
            val elapsedTime = System.currentTimeMillis() - startTime
            if (elapsedTime > PROPAGATION_TIMEOUT_MS) {
              debugPrint(s"TIMEOUT: naiveZonk exceeded time limit of ${PROPAGATION_TIMEOUT_MS}ms (elapsed: ${elapsedTime}ms)")
              setDidSomething(true)
              loop = false
              // Don't throw exception on timeout - just apply fallbacks if possible
              tryFallback = 2 // Trigger fallback on timeout
              break
            }

            // Break if iterations are excessive
            if (iterationCount >= 10) {
              debugPrint(s"Too many iterations in naiveZonk, forcing exit")
              setDidSomething(true)
              loop = false
              break
            }

            setDidSomething(false)

            // Process any remaining propagators
            debugPrint("Processing remaining propagators with tick")
            tick

            // Extra safety check: filter out cells that might have become null due to concurrency
            cellsNeeded = cellsNeeded.filter(_ != null).filter(this.noAnyValue(_))
            debugPrint(s"After filtering, ${cellsNeeded.size} cells still need values")

            if (cellsNeeded.isEmpty) {
              debugPrint("All cells have values, exiting naiveZonk")
              loop = false
              break
            } else {
              // Process cells that still need values
              val tasks = cellsNeeded.flatMap { c =>
                if (c != null && c.noAnyValue) {
                  // Safety check: ensure we can access zonking propagators
                  try {
                    // Limit to top propagators by score
                    val alivePropagators = c.zonkingPropagators.asScala
                      .filter(_ != null)
                      .filter(_.alive)
                      .toVector
                      .sortBy(p => -p.store.score)

                    debugPrint(s"Cell $c has ${alivePropagators.size} alive propagators")

                    // Limit number of propagators we try per cell
                    val limitedPropagators = if (alivePropagators.size > 3) {
                      alivePropagators.take(3)
                    } else {
                      alivePropagators
                    }

                    limitedPropagators.map { p =>
                      debugPrint(s"Creating ZonkTask for cell $c with propagator ${p.store.getClass.getSimpleName}")
                      new ZonkTask(
                        c,
                        p.asInstanceOf[HoldPropagator[Propagator[Ability]]],
                        firstFallback = tryFallback == 0,
                        this,
                        currentDepth,
                        processedCellIds
                      )
                    }
                  } catch {
                    case e: Exception =>
                      debugPrint(s"Exception accessing zonking propagators for cell $c: ${e.getMessage}")
                      e.printStackTrace()
                      Vector.empty
                  }
                } else {
                  Vector.empty
                }
              }

              debugPrint(s"Created ${tasks.size} zonk tasks")
              if (tasks.nonEmpty) {
                try {
                  debugPrint("Invoking zonk tasks")
                  // Add timeout for task execution
                  val taskFuture = ForkJoinTask.invokeAll(tasks.asJava)
                  tick
                  debugPrint("Zonk tasks completed and tick processed")
                } catch {
                  case e: Exception => 
                    // Log exception but continue processing
                    debugPrint(s"Exception during zonk task execution: ${e.getMessage}")
                    e.printStackTrace()
                    setDidSomething(true)
                }
              }

              // Check if we made progress
              if (!didSomething) {
                tryFallback += 1
                debugPrint(s"No progress made, increasing fallback to $tryFallback")
              } else {
                // Reset fallback counter if we made progress
                tryFallback = 0
                // Update cells list
                debugPrint("Progress made, resetting fallback counter")
                cellsNeeded = cellsNeeded.filter(_ != null).filter(_.noAnyValue)
                debugPrint(s"After progress, ${cellsNeeded.size} cells still need values")
                if (cellsNeeded.isEmpty) {
                  debugPrint("All cells have values after progress, exiting naiveZonk")
                  loop = false
                  break
                }
              }
            }
          }
        } // end breakable

        // If still not resolved, try default values
        if (cellsNeeded.nonEmpty && tryFallback > 1) {
          debugPrint(s"Trying default values for ${cellsNeeded.size} unresolved cells")
          val defaultTasks = cellsNeeded.flatMap { c =>
            Option.when(c != null && c.noAnyValue && c.store.default.isDefined) {
              debugPrint(s"Creating DefaultValueTask for cell $c with default value ${c.store.default}")
              new DefaultValueTask(c, this)
            }
          }

          if (defaultTasks.nonEmpty) {
            debugPrint(s"Executing ${defaultTasks.size} default value tasks")
            try {
              val _ = ForkJoinTask.invokeAll(defaultTasks.asJava)
              // Final check for unresolved cells
              cellsNeeded = cellsNeeded.filter(_ != null).filter(_.noAnyValue)
              debugPrint(s"After default values, ${cellsNeeded.size} cells still unresolved")
            } catch {
              case e: Exception =>
                // Continue even if there's an exception
                debugPrint(s"Exception during default value task execution: ${e.getMessage}")
                e.printStackTrace()
                setDidSomething(true)
            }
          }

          // One final safety check - any cells with default values we can still apply?
          val lastChanceCells = cellsNeeded.filter(_ != null).filter(c => c.noAnyValue && c.store.default.isDefined)
          if (lastChanceCells.nonEmpty) {
            debugPrint(s"LAST CHANCE: Applying default values to ${lastChanceCells.size} cells directly")
            lastChanceCells.foreach { c =>
              try {
                fill(c.asInstanceOf[CIdOf[Cell[Any]]], c.store.default.get)
              } catch {
                case e: Exception =>
                  debugPrint(s"Exception applying last chance default value to cell $c: ${e.getMessage}")
              }
            }
            // Update cellsNeeded one last time
            cellsNeeded = cellsNeeded.filter(_ != null).filter(_.noAnyValue)
          }

          // Throw exception for cells that couldn't be resolved
          if (cellsNeeded.nonEmpty) {
            debugPrint(s"FATAL: Cells that couldn't be resolved: $cellsNeeded")
            throw new IllegalStateException(
              s"Cells $cellsNeeded are not covered by any propagator"
            )
          }
        }
      } catch {
        case e: IllegalStateException => 
          // Rethrow illegal state exceptions
          debugPrint(s"IllegalStateException in naiveZonk: ${e.getMessage}")
          throw e
        case e: Exception =>
          // Log other exceptions but allow processing to continue
          debugPrint(s"Exception in naiveZonk: ${e.getMessage}")
          e.printStackTrace()
          setDidSomething(true)
      } finally {
        decrementRecursionDepth()
        debugPrint(s"naiveZonk finished at depth ${getRecursionDepth()}")
      }
    }

    class BatchPropagatorTask(
        propagators: Vector[PIdOf[Propagator[Ability]]],
        state: Impl[Ability]
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit = {
        try {
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
        } catch {
          case ex: Exception =>
            // Just mark as doing something and continue
            setDidSomething(true)
        }
      }
    }

    class PropagatorTask(
        propagators: Vector[PIdOf[Propagator[Ability]]],
        state: Impl[Ability]
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit = {
        try {
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
        } catch {
          case ex: Exception =>
            // Just mark as doing something and continue
            setDidSomething(true)
        }
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
            debugPrint(s"ZonkTask: Recursion depth $parentRecursionDepth exceeded limit - terminating")
            setDidSomething(true)
            return
          }

          val propagator = p.store
          debugPrint(s"ZonkTask: Running propagator ${propagator.getClass.getSimpleName} for cell $c")
          val zonkResult =
            try {
              if (firstFallback) {
                debugPrint("ZonkTask: Using naiveZonk")
                propagator.naiveZonk(Vector(c))(using state, more)
              } else {
                debugPrint("ZonkTask: Using naiveFallbackZonk")
                propagator.naiveFallbackZonk(Vector(c))(using state, more)
              }
            } catch {
              case e: Exception => 
                // Continue with NotYet result on exception
                debugPrint(s"ZonkTask: Exception during propagator execution: ${e.getMessage}")
                e.printStackTrace()
                ZonkResult.NotYet
            }

          debugPrint(s"ZonkTask: Propagator result: $zonkResult")
          zonkResult match {
            case ZonkResult.Done =>
              debugPrint("ZonkTask: Propagator done, marking as inactive")
              p.setAlive(false)
              val _ = propagators.remove(p)
              setDidSomething(true)
            case ZonkResult.Require(needed) =>
              debugPrint(s"ZonkTask: Propagator requires ${needed.size} additional cells")
              // Filter out cells we've already seen to prevent cycles
              val newNeeded = needed.toVector
                .filter(n => n.noStableValue)
                .filterNot(n => n == c)
                .filterNot(n => processedCellIds.contains(n.toString))

              debugPrint(s"ZonkTask: After filtering, ${newNeeded.size} new cells needed")
              if (newNeeded.nonEmpty) {
                // Mark these cells as processed before recursing
                newNeeded.foreach(n => processedCellIds.add(n.toString))
                // Process the newly needed cells
                try {
                  debugPrint(s"ZonkTask: Recursively processing ${newNeeded.size} new cells")
                  state.naiveZonk(newNeeded)
                  setDidSomething(true)
                } catch {
                  case e: Exception =>
                    // Just mark as doing something and continue
                    debugPrint(s"ZonkTask: Exception during recursive naiveZonk: ${e.getMessage}")
                    e.printStackTrace()
                    setDidSomething(true)
                }
              }
            case ZonkResult.NotYet =>
              debugPrint("ZonkTask: Propagator not yet ready")
            // No action needed
          }
        } else {
          debugPrint(s"ZonkTask: Cell $c either has a value or propagator ${p} is not alive")
        }
      }
    }

    class DefaultValueTask(c: CIdOf[Cell[?]], state: Impl[Ability])(using
        Ability
    ) extends RecursiveAction {
      override def compute(): Unit = {
        if (c.noAnyValue && c.store.default.isDefined) {
          state.fill(c.asInstanceOf[CIdOf[Cell[Any]]], c.store.default.get)
          setDidSomething(true)
        }
      }
    }
  }
}
