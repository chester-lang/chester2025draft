package chester.eval

import chester.runtime.Value
import chester.syntax.core.*
import chester.syntax.core.spec.{BooleanTermC, TermT}

case class EvalContext()

case class Eval[Term <: TermT[Term]]() {
  type BooleanTerm = BooleanTermC[Term]
  def evalNoEffect(code: Term, ctx: EvalContext = EvalContext()): Value = code match {
    case b: BooleanTerm => Value(b.value)
    case _              => ???
  }
}
