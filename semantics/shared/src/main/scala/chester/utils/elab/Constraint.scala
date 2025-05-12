package chester.utils.elab

import chester.syntax.core.Term

/** implementations should be case object */
open trait Kind {
  def name: String = toString
  type ConstraintType <: Constraint
}

open trait Constraint(val kind: Kind) {
  def show: Vector[Term] = ???
}

enum Result {
  case Done
  case Failed
  case Waiting(vars: Vector[CellId[?]])
}

open trait Handler[Ops](val kind: Kind) {
  def run(constant: kind.ConstraintType)(using Ops, SolverOps): Result = ???
  def zonk(constant: kind.ConstraintType, level: ZonkLevel)(using Ops, SolverOps): Unit = ()
}

import scala.collection.concurrent.TrieMap

trait HandlerConf[Ops] {
  def getHandler(kind: Kind): Option[Handler[Ops]]
}

final class MutHandlerConf[Ops](hs: Handler[Ops]*) extends HandlerConf[Ops] {
  private val store = TrieMap[Kind, Handler[Ops]](hs.map(h => (h.kind, h))*)

  override def getHandler(kind: Kind): Option[Handler[Ops]] = store.get(kind)

  def register(handler: Handler[Ops]): Unit = {
    val oldValue = store.putIfAbsent(handler.kind, handler)
    if (oldValue.isDefined) throw new IllegalStateException("already")
  }
}

enum ZonkLevel extends Enum[ZonkLevel] {
  case First
  case ZonkEverything
}
object ZonkLevel {
  val Values: Vector[ZonkLevel] = ZonkLevel.values.toVector.sortBy(_.precedence)
}
extension (x: ZonkLevel) {
  // depend on this assumption: the first one should be 0
  def precedence: Int = x.ordinal
}
