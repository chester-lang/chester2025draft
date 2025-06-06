package chester.elab

import chester.syntax.TreeMap
import chester.syntax.core.*
import chester.utils.elab.SolverOps

implicit class ZonkAllOnTerm[T <: Term](val t: T) extends AnyVal {
  def readMetaAll(using SolverOps): t.ThisTree = t
    .descentRec {
      case t: MetaTerm[?] =>
        // introduce a variable for easy breakpoint
        val result = toTermUnstable(t)
        result match {
          case _: MetaTerm[?] =>
            // newline for easy breakpoint
            throw new IllegalStateException("Zonked term is still a MetaTerm")
          case t: Term => t.readMetaAll
        }
      case t: Term => t
    }
    .asInstanceOf[t.ThisTree]

  def zonkAll(using SolverOps, Context, Elab): Term = readMetaAll.descent2Rec(TreeMap.unsafe {
    case x: LetStmtTerm =>
      x.copy(ty = summon[Elab].reduceNoEffectUntyped(x.ty))
    case x: ArgTerm      => x.copy(ty = summon[Elab].reduceNoEffectUntyped(x.ty))
    case x: FunctionType => x.copy(resultTy = summon[Elab].reduceNoEffectUntyped(x.resultTy))
    case x               => x
  })
}

implicit class ZonkAllOnTAST(private val tast: TAST) extends AnyVal {
  def readMetaAll(using SolverOps): ZonkedTAST =
    tast
      .copy(
        ty = tast.ty.readMetaAll
      )
      .zonked(
        ast = TAST.termToBlockNoMeta(tast.ast.readMetaAll),
        effects = tast.effects.readMetaAll.assumeEffects
      )
  def zonkAll(using SolverOps, Context, Elab): ZonkedTAST = {
    val result = readMetaAll
    result.zonked(ast = result.ast.zonkAll.assumeBlock, effects = result.effects.zonkAll.assumeEffects)
  }
}

extension (t: Term) {
  def assumeEffects: Effects = t match {
    case e: Effects => e
    case _          => throw new IllegalArgumentException(s"Expected Effects, but got: $t")
  }
  def assumeBlock: BlockTerm = t match {
    case b: BlockTerm => b
    case _            => throw new IllegalArgumentException(s"Expected Block, but got: $t")
  }
}
