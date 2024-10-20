package chester.utils.propagator

import chester.uniqid.{UniqId, UniqIdOf}

import scala.collection.mutable

trait ProvideMutable extends ProvideImpl {
  class HoldCell[+T <: Cell[?]](val uniqId: UniqIdOf[Impl[?]], value: T) {
    var store: Cell[?] = value
    var didChange: Boolean = false
    var readingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty
    var zonkingPropagators: Vector[PIdOf[Propagator[?]]] = Vector.empty

    inline def noAnyValue: Boolean = store.noAnyValue
  }

  type CIdOf[+T <: Cell[?]] = HoldCell[T]

  class HoldPropagator[+T <: Propagator[?]](
      val uniqId: UniqIdOf[Impl[?]],
      value: T
  ) {
    var store: Propagator[?] = value
    var alive: Boolean = true
  }

  type PIdOf[+T <: Propagator[?]] = HoldPropagator[T]

  override def isCId(x: Any): Boolean = {
    val result = x.isInstanceOf[HoldCell[?]]
    result
  }

  override def assumeCId(x: Any): CIdOf[Cell[?]] = {
    require(isCId(x))
    x.asInstanceOf[CIdOf[Cell[?]]]
  }

  override def stateAbilityImpl[Ability]: StateAbility[Ability] =
    Impl[Ability]()

  class Impl[Ability](
      val uniqId: UniqIdOf[Impl[Ability]] = UniqId.generate[Impl[Ability]]
  ) extends StateAbility[Ability] {
    var didChanged: mutable.ArrayDeque[CIdOf[?]] = mutable.ArrayDeque.empty

    override def readCell[T <: Cell[?]](id: CIdOf[T]): Option[T] = {
      require(id.uniqId == uniqId)
      Some(id.store.asInstanceOf[T])
    }

    override def update[T <: Cell[?]](id: CIdOf[T], f: T => T)(using
        Ability
    ): Unit = {
      didSomething = true
      require(id.uniqId == uniqId)
      id.store = f(id.store.asInstanceOf[T])
      id.didChange = true
      didChanged.append(id)
    }

    override def addCell[T <: Cell[?]](cell: T): CIdOf[T] = {
      didSomething = true
      val id = new HoldCell[T](uniqId, cell)
      id
    }

    override def addPropagator[T <: Propagator[Ability]](
        propagator: T
    )(using more: Ability): PIdOf[T] = {
      didSomething = true
      val id = new HoldPropagator[T](uniqId, propagator)
      for (cell <- propagator.zonkingCells) {
        cell.zonkingPropagators = cell.zonkingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      }
      for (cell <- propagator.readingCells) {
        cell.readingPropagators = cell.readingPropagators :+ id.asInstanceOf[PIdOf[Propagator[?]]]
      }
      if (propagator.run(using this, more)) {
        id.alive = false
      }
      id
    }

    override def stable: Boolean = didChanged.isEmpty

    override def tick(using more: Ability): Unit = {
      while (didChanged.nonEmpty) {
        val id = didChanged.removeHead()
        if (id.didChange) {
          id.didChange = false
          for (p <- id.readingPropagators) {
            require(p.uniqId == uniqId)
            if (p.alive) {
              if (p.store.asInstanceOf[Propagator[Ability]].run(using this, more)) {
                didSomething = true
                p.alive = false
              }
            }
          }
        }
      }
    }

    @deprecated("impure")
    override def readingZonkings(
        cells: Vector[CIdOf[Cell[?]]]
    ): Vector[Propagator[Ability]] = {
      cells
        .flatMap(_.zonkingPropagators)
        .map(_.store.asInstanceOf[Propagator[Ability]])
    }

    @deprecated("impure")
    override def requireRemovePropagatorZonking(
        identify: Any,
        cell: CellIdAny
    ): Unit = {
      val cell1 = cell.asInstanceOf[CIdOf[Cell[?]]]
      for (
        p <- cell1.zonkingPropagators
          .filter(x => x.store.identify == Some(identify))
      ) {
        if (p.alive) {
          didSomething = true
          p.alive = false
        }
      }
    }

    var didSomething = false

    override def naiveZonk(
        cells: Vector[CIdOf[Cell[?]]]
    )(using more: Ability): Unit = {
      var cellsNeeded = cells
      var tryFallback: Int = 0
      while (true) {
        didSomething = false
        tickAll
        cellsNeeded = cellsNeeded
          .filter(this.noAnyValue(_))
          .sortBy(x => -x.zonkingPropagators.map(_.store.score).sum)
        if (cellsNeeded.isEmpty) {
          return
        }
        for (c <- cellsNeeded) {
          require(c.uniqId == uniqId)
          if (c.noAnyValue) {
            def processZonking = {
              val aliveP = c.zonkingPropagators.filter(_.alive)
              c.zonkingPropagators = aliveP
              val zonking = aliveP.sortBy(x => -x.store.score)
              zonking
            }
            for (p <- processZonking) {
              require(p.uniqId == uniqId)
              tickAll
              if (c.noAnyValue && p.alive) {
                val store = p.store.asInstanceOf[Propagator[Ability]]
                if (store.run(using this, more)) {
                  p.alive = false
                  didSomething = true
                } else {
                  val on = store.naiveZonk(cellsNeeded)(using this, more)
                  on match {
                    case ZonkResult.Done =>
                      p.alive = false
                      didSomething = true
                    case ZonkResult.Require(needed) =>
                      val needed1 = needed
                        .filter(this.noStableValue(_))
                        .filterNot(cellsNeeded.contains)
                      if (needed1.nonEmpty) {
                        cellsNeeded = cellsNeeded ++ needed1
                        didSomething = true
                      }
                    case ZonkResult.NotYet =>
                  }
                }
              }
            }
            if (tryFallback > 0 && !didSomething) {
              for (p <- processZonking) {
                require(p.uniqId == uniqId)
                tickAll
                if (c.noAnyValue && p.alive) {
                  val store = p.store.asInstanceOf[Propagator[Ability]]
                  if (store.run(using this, more)) {
                    p.alive = false
                    didSomething = true
                  } else {
                    val on =
                      store.naiveFallbackZonk(cellsNeeded)(using this, more)
                    on match {
                      case ZonkResult.Done =>
                        p.alive = false
                        didSomething = true
                      case ZonkResult.Require(needed) =>
                        val needed1 = needed
                          .filter(this.noStableValue(_))
                          .filterNot(cellsNeeded.contains)
                        if (needed1.nonEmpty) {
                          cellsNeeded = cellsNeeded ++ needed1
                          didSomething = true
                        }
                      case ZonkResult.NotYet =>
                    }
                  }
                }
              }
            }
          }
        }
        if (tryFallback > 1 && !didSomething) {
          for (c <- cellsNeeded) {
            if (c.noAnyValue && c.store.default.isDefined) {
              fill(c.asInstanceOf[CellId[Any]], c.store.default.get)
              didSomething = true
            }
          }
        }
        if (!didSomething) {
          if (tryFallback > 1) {
            throw new IllegalStateException(
              s"Cells $cellsNeeded are not covered by any propagator"
            )
          } else {
            tryFallback = tryFallback + 1
          }
        } else {
          tryFallback = 0
        }
      }
      tickAll
    }
  }
}
