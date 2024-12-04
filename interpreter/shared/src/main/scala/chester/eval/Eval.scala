package chester.eval

import chester.runtime.Value
import chester.syntax.core.TermT

case class EvalContext()

object Eval {
  def evalNoEffect[T<:TermT[T]](ctx: EvalContext, code: T): Value = ???
}
