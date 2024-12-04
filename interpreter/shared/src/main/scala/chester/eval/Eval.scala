package chester.eval

import chester.runtime.Value
import chester.syntax.core.*

case class EvalContext()

case class Eval[Term<:TermT[Term]]() {
  type BooleanTerm = BooleanTermC[Term]
  def evalNoEffect( code: Term, ctx: EvalContext =  EvalContext()): Value = code match {
    case b: BooleanTerm => Value(b.value)
    case _ => ???
  }
}
