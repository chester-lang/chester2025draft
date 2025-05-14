package chester.utils.elab

import chester.utils.cell.{CellContent, CellContentR}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

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
   val todo = mutable.Queue[Constraint]()
   val delayedConstraints = mutable.ArrayBuffer[WaitingConstraint]()
   val failedConstraints = mutable.ArrayBuffer[Constraint]()
   val updatedCells = mutable.ArrayBuffer[CellAny]()

  implicit inline def thereAreAllProcedural[A, B, C <: CellContent[A, B]](inline x: Cell[A, B, C]): ProceduralCell[A, B, C] =
    x.asInstanceOf[ProceduralCell[A, B, C]]

  override protected def peakCell[T](id: CellR[T]): CellContentR[T] = id.store

  override protected def updateCell[A, B](id: CellOf[A, B], f: CellContent[A, B] => CellContent[A, B]): Unit = {
    id.store = f(id.store)
    updatedCells.append(id)
  }

  @tailrec
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
        val _ = delayedConstraints.filterInPlace { c =>
          val call = c.vars.exists(updatedCells.contains)
          if (call) todo.enqueue(c.x)
          !call
        }
        updatedCells.clear()
      }
    }
    var defaults = DefaultingLevel.Values
    var nothingChanged = true
    while (nothingChanged && defaults.nonEmpty) {
      val default = defaults.head
      defaults = defaults.tail
      val _ = delayedConstraints.flatMapInPlace { x =>
        val c = x.x
        val handler = conf.getHandler(c.kind).getOrElse(throw new IllegalStateException("no handler"))
        val result = handler.run(c.asInstanceOf[handler.kind.Of])
        result match {
          case Result.Done =>
            nothingChanged = false
            Vector()
          case Result.Failed =>
            nothingChanged = false
            failedConstraints.append(c)
            Vector()
          case Result.Waiting(vars*) =>
            handler.defaulting(c.asInstanceOf[handler.kind.Of], default)
            val result = handler.run(c.asInstanceOf[handler.kind.Of])
            result match {
              case Result.Done =>
                nothingChanged = false
                Vector()
              case Result.Failed =>
                nothingChanged = false
                failedConstraints.append(c)
                Vector()
              case Result.Waiting(vars*) =>
                Vector(WaitingConstraint(vars.toVector, c))
            }
        }
      }
      if(updatedCells.nonEmpty || todo.nonEmpty)nothingChanged = false
    }
    if (defaults.isEmpty && nothingChanged) {
      throw new IllegalStateException("cannot finish some constraints")
    }
      val _ = delayedConstraints.filterInPlace { c =>
        val call = c.vars.exists(updatedCells.contains)
        if (call) todo.enqueue(c.x)
        !call
      }
      updatedCells.clear()
    if (!stable) return run()
  }

  override def stable: Boolean = delayedConstraints.isEmpty && todo.isEmpty

  override def addConstraint(x: Constraint): Unit = todo.enqueue(x)

  override def addCell[A, B, C <: CellContent[A, B]](cell: C): Cell[A, B, C] = ProceduralCell(cell)
}
