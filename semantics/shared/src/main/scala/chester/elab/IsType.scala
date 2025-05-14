package chester.elab

import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*

case object IsType extends Kind {
  type Of = IsType
}

case class IsType(result: CellRW[Term])(using ctx: Context)
    extends Constraint(IsType)
    with ConstraintTermRW {
  given Context = ctx
}

case object IsTypeHandler extends Handler[ElabOps, IsType.type](IsType) {
  override def run(c: IsType)(using ElabOps, SolverOps): Result = {
    import c.*
    toTerm(result) match {
      case _: MetaTerm => Result.Waiting(assumeCell(result))
      case _ => Result.Done
    }
  }

  override def defaulting(constant: IsType, level: DefaultingLevel)(using ElabOps, SolverOps): Unit = {
    if(level != DefaultingLevel.DefaultingEverything) return
    import constant.*
    toTerm(result) match {
      case result: MetaTerm => {
        assumeCell(result).fill(AnyType0)
      }
      case _ => {
        // do nothing
      }
    }
  }
}
