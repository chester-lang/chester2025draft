package chester.elab

import chester.syntax.core.{Effects, MetaTerm, Term}
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
  def zonkAll(using ops: SolverOps): ZonkedTAST =
    tast
      .copy(
        ty = tast.ty.zonkAll
      )
      .zonked(
        ast = TAST.termToBlockNoMeta(tast.ast.zonkAll),
        effects = tast.effects.zonkAll.assumeEffects
      )
}

extension (t: Term) {
  def assumeEffects: Effects = t match {
    case e: Effects => e
    case _          => throw new IllegalArgumentException(s"Expected Effects, but got: $t")
  }
}
