package chester.eval

import chester.syntax.core.*

case class ReduceContext()

trait Reducer {
  def reduce(term: Term)(using ReduceContext, Reducer): Term
}

object Reducer {
  def reduce(term: Term)(using ctx:ReduceContext, r: Reducer): Term = r.reduce(term)
}

object NaiveReducer extends Reducer {
  override def reduce(term: Term)(using ReduceContext, Reducer):Term=term
}