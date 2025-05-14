package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR}

import scala.collection.mutable

final class ProceduralCell[+A, -B, C <: CellContent[A, B]](
    initialValue: C
) extends Cell[A, B, C] {
  var store = initialValue
}

object ProceduralSolver extends SolverFactory {
  override def apply[Ops](conf: HandlerConf[Ops])(using Ops): SolverOps = new ProceduralSolver(conf)
}

final class ProceduralSolver[Ops](val conf: HandlerConf[Ops])(using Ops) extends BasicSolverOps {
  given SolverOps = this
  private val todo = mutable.Queue[Constraint]()
  private var delayedConstraints = mutable.ArrayBuffer[WaitingConstraint]()
  private val failedConstraints = mutable.ArrayBuffer[Constraint]()
  private val updatedCells = mutable.ArrayBuffer[CellAny]()

  implicit inline def thereAreAllProcedural[A, B, C <: CellContent[A, B]](inline x: Cell[A, B, C]): ProceduralCell[A, B, C] =
    x.asInstanceOf[ProceduralCell[A, B, C]]

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = id.store

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    id.store = f(id.store)
    updatedCells.append(id)
  }

  // @tailrec
  override def run(): Unit = {
    while (todo.nonEmpty) {
      var heuristics: Int = 1
      while (todo.nonEmpty && heuristics < 32) {
        heuristics += 1
        val c = todo.dequeue()
        val handler = conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler"))
        val result = handler.run(c.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done =>
          // do nothing
          case Result.Waiting(vars*) =>
            delayedConstraints.append(WaitingConstraint(vars.toVector, c))
          case Result.Failed =>
            failedConstraints.append(c)
        }
      }
      if (delayedConstraints.nonEmpty) {
        // pick those has waiting vars in updatedCells
        val _ = delayedConstraints.filterInPlace { c =>
          val call = c.vars.exists(updatedCells.contains)
          if (call) todo.enqueue(c.x)
          !call
        }
        updatedCells.clear()
      }
    }
    while (delayedConstraints.nonEmpty)
      // TODO: defaulting
      ???
  }

  override def stable: Boolean = delayedConstraints.isEmpty && todo.isEmpty

  override def addConstraint(x: Constraint): Unit = todo.enqueue(x)

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ProceduralCell(cell)
}
