package chester.utils.propagator

import chester.uniqid.*
import chester.i18n.*
import chester.utils.cell.*

trait ProvideImmutable extends ProvideImpl {
  type CIdOf[+T <: Cell[?, ?]] = UniqidOf[T]
  type PIdOf[+T <: Propagator[?]] = UniqidOf[T]
  override def isCId(x: Any): Boolean = Uniqid.is(x)

  type CellsState = Map[CIdOf[Cell[?, ?]], Cell[?, ?]]
  private val CellsStateEmpty: CellsState = Map.empty
  type PropagatorsState[Ability] =
    Map[PIdOf[Propagator[Ability]], Propagator[Ability]]

  private inline def PropagatorsStateEmpty[Ability]: PropagatorsState[Ability] =
    Map.empty

  case class State[Ability](
      cells: CellsState = CellsStateEmpty,
      propagators: PropagatorsState[Ability] = PropagatorsStateEmpty[Ability],
      didChanged: Vector[CIdOf[Cell[?, ?]]] = Vector.empty
  ) {
    def stable: Boolean = didChanged.isEmpty
  }

  override def stateAbilityImpl[Ability]: StateOps[Ability] =
    StateCells[Ability]()

  class StateCells[Ability](var state: State[Ability] = State[Ability]()) extends StateOps[Ability] {
    override def stable: Boolean = state.stable

    override def readCell[T <: Cell[?, ?]](id: CIdOf[T]): Option[T] =
      state.cells.get(id).asInstanceOf[Option[T]]

    override def update[T <: Cell[?, ?]](id: CIdOf[T], f: T => T)(using
                                                                  Ability
    ): Unit =
      state.cells.get(id) match {
        case Some(cell) =>
          val newCell = f(cell.asInstanceOf[T])
          if (cell != newCell) {
            state = state.copy(
              didChanged = state.didChanged :+ id,
              cells = state.cells.updated(id, newCell)
            )
          }
        case None =>
          throw new IllegalArgumentException(t"Cell with id $id not found")
      }

    override def addCell[T <: Cell[?, ?]](cell: T): CIdOf[T] = {
      val id = Uniqid.generate[T]
      state = state.copy(cells = state.cells.updated(id, cell))
      id
    }

    override def addPropagatorGetPid[T <: Propagator[Ability]](
        propagator: T
    )(using more: Ability): PIdOf[T] = {
      val uniqId = Uniqid.generate[T]
      state = state.copy(propagators = state.propagators.updated(uniqId, propagator))
      if (propagator.run(using this, more)) {
        state = state.copy(propagators = state.propagators.removed(uniqId))
      }
      uniqId
    }

    override def tick(using more: Ability): Unit = {
      given StateOps[Ability] = this
      val didChanged = state.didChanged
      state = state.copy(didChanged = Vector.empty)
      state.propagators
        .withFilter((_, propagator) => propagator.readingCells.exists(didChanged.contains))
        .foreach { case (pid, propagator) =>
          if (state.propagators.contains(pid)) {
            val done = propagator.run(using this, more)
            if (done) {
              state = state.copy(propagators = state.propagators.removed(pid))
            }
          }
        }
    }

    override def defaulting(
        cells: Vector[CIdOf[Cell[?, ?]]]
    )(using more: Ability): Unit = {
      given StateOps[Ability] = this
      var cellsNeeded = Vector.empty[CIdOf[Cell[?, ?]]]
      while (true) {
        tickAll
        val cellsToZonk = if (cellsNeeded.nonEmpty) {
          val a = cellsNeeded
          cellsNeeded = Vector.empty
          (a ++ cells).filterNot(id => state.cells(id).hasStableValue)
        } else {
          cells.filterNot(id => state.cells(id).hasStableValue)
        }
        val xs = state.propagators.filter((_, propagator) => propagator.defaultingCells.exists(cellsToZonk.contains))
        val uncorvedCells = cellsToZonk.filterNot(id => xs.values.exists(_.defaultingCells.contains(id)))
        if (uncorvedCells.nonEmpty) {
          throw new IllegalStateException(
            t"Cells $uncorvedCells are not covered by any propagator"
          )
        }
        xs.foreach { case (pid, propagator) =>
          tickAll
          if (state.propagators.contains(pid)) {
            val result = propagator.defaulting(cells)(using this, more)
            result match {
              case DefaultingResult.Done =>
                state = state.copy(propagators = state.propagators.removed(pid))
              case DefaultingResult.Require(needed) =>
                cellsNeeded = cellsNeeded ++ needed
              case DefaultingResult.NotYet =>
            }
          }
        }
        tickAll
        if (cellsToZonk.isEmpty) return
      }
    }
  }

}
