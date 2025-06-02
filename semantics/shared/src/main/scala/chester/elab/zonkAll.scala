package chester.elab

import chester.syntax.core.{MetaTerm, Term}
import chester.utils.elab.SolverOps

implicit class ZonkAllOnTerm[T <: Term](val t: T) {
  def zonkAll(using ops: SolverOps): t.ThisTree = t
    .descentRec {
      case t: MetaTerm =>
        // introduce a variable for easy breakpoint
        val result = toTerm(t)
        result match {
          case _: MetaTerm =>
            // newline for easy breakpoint
            throw new IllegalStateException("Zonked term is still a MetaTerm")
          case t: Term => t.zonkAll
        }
      case t: Term => t
    }
    .asInstanceOf[t.ThisTree]
}

implicit class ZonkAllOnTAST(tast: TAST) {
  def zonkAll(using ops: SolverOps): TAST =
    tast.copy(
      ast = TAST.termToBlock(tast.ast.zonkAll),
      effects = tast.effects.zonkAll,
      ty = tast.ty.zonkAll
    )
}
