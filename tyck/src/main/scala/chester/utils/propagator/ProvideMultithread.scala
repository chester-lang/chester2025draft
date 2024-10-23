package chester.utils.propagator

import chester.uniqid.{UniqId, UniqIdOf}

import java.util.concurrent.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

// currently broken too eager to use default values
trait ProvideMultithread extends ProvideImpl {

  class HoldCell[+T <: Cell[?]](
      val uniqId: UniqIdOf[Impl[?]],
      initialValue: T
  ) {
    private val storeRef = new AtomicReference[Cell[?]](initialValue)
    val readingPropagators = new ConcurrentLinkedQueue[PIdOf[Propagator[?]]]()
    val zonkingPropagators = new ConcurrentLinkedQueue[PIdOf[Propagator[?]]]()

    def store: T = storeRef.get().asInstanceOf[T]

    /** Atomically updates the store */
    def compareAndSetStore(
        expectedValue: Cell[?],
        newValue: Cell[?]
    ): Boolean = {
      storeRef.compareAndSet(expectedValue, newValue)
    }

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
      val uniqId: UniqIdOf[Impl[?]],
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
      val uniqId: UniqIdOf[Impl[Ability]] = UniqId.generate[Impl[Ability]]
  ) extends StateAbility[Ability] {

    private val propagators =
      new ConcurrentLinkedQueue[PIdOf[Propagator[Ability]]]()
    private val forkJoinPool = new ForkJoinPool(
      Runtime.getRuntime.availableProcessors()
    )

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
          // Immediately process dependent propagators
          processPropagators(id)
        }
      }
    }

    override def tick(using Ability): Unit = {
      // Do nothing
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
          // Immediately process dependent propagators
          processPropagators(id)
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
      // Submit task to process this propagator
      forkJoinPool.execute(
        new PropagatorTask(
          Vector(id.asInstanceOf[HoldPropagator[Propagator[Ability]]]),
          this
        )
      )
      id
    }

    override def stable: Boolean = propagators.isEmpty

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
      var cellsNeeded = cells
      var tryFallback = 0
      var loop = true

      while (loop) {
        // Process any remaining propagators
        val tasks = cellsNeeded.flatMap { c =>
          if (c.noAnyValue) {
            c.zonkingPropagators.asScala.filter(_.alive).map { p =>
              new ZonkTask(
                c,
                p.asInstanceOf[HoldPropagator[Propagator[Ability]]],
                firstFallback = tryFallback == 0,
                this
              )
            }
          } else {
            Vector.empty
          }
        }
        if (tasks.nonEmpty) {
          val _ =ForkJoinTask.invokeAll(tasks.asJava)
        }

        // Check if all cells have values
        cellsNeeded = cellsNeeded.filter(_.noAnyValue)
        if (cellsNeeded.isEmpty) {
          loop = false
        } else {
          tryFallback += 1
          if (tryFallback > 2) {
            // Last resort: try default values
            val defaultTasks =
              cellsNeeded.map(c => new DefaultValueTask(c, this))
            val _ =ForkJoinTask.invokeAll(defaultTasks.asJava)

            // Check again if all cells have values
            cellsNeeded = cellsNeeded.filter(_.noAnyValue)
            if (cellsNeeded.nonEmpty) {
              throw new IllegalStateException(
                s"Cells $cellsNeeded are not covered by any propagator"
              )
            } else {
              loop = false
            }
          }
        }
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
                state.propagators.remove(p)
              }
            }
          }
        } catch {
          case ex: Exception =>
            // Handle or log the exception as needed
            throw ex
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
                state.propagators.remove(p)
              }
            }
          }
        } catch {
          case ex: Exception =>
            // Handle or log the exception as needed
            throw ex
        }
      }
    }

    class ZonkTask(
        c: CIdOf[Cell[?]],
        p: PIdOf[Propagator[Ability]],
        firstFallback: Boolean,
        state: Impl[Ability]
    )(using more: Ability)
        extends RecursiveAction {
      override def compute(): Unit = {
        require(c.uniqId == uniqId)
        require(p.uniqId == uniqId)
        if (c.noAnyValue && p.alive) {
          val propagator = p.store
          val zonkResult = if (firstFallback) {
            propagator.naiveZonk(Vector(c))(using state, more)
          } else {
            propagator.naiveFallbackZonk(Vector(c))(using state, more)
          }
          zonkResult match {
            case ZonkResult.Done =>
              p.setAlive(false)
              propagators.remove(p)
            case ZonkResult.Require(needed) =>
              needed.foreach { n =>
                if (n.noStableValue) {
                  // Process the newly needed cells
                  state.naiveZonk(Vector(n))
                }
              }
            case ZonkResult.NotYet =>
            // Do nothing
          }
        }
      }
    }

    class DefaultValueTask(c: CIdOf[Cell[?]], state: Impl[Ability])(using
        ability: Ability
    ) extends RecursiveAction {
      override def compute(): Unit = {
        if (c.noAnyValue && c.store.default.isDefined) {
          state.fill(c.asInstanceOf[CIdOf[Cell[Any]]], c.store.default.get)
        }
      }
    }
  }
}
