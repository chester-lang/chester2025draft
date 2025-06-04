package chester.elab

import cats.data.NonEmptyVector
import chester.syntax.core.*
import chester.elab.Context
import chester.utils.*
import chester.utils.elab.*

case object SimplifyUnion extends Kind {
  type Of = SimplifyUnion

}

case class SimplifyUnion(items: NonEmptyVector[CellROr[Term]], meta: Option[TermMeta] = None)(using ctx: Context, ops: SolverOps)
    extends Constraint(SimplifyUnion)
    with ConstraintTerm {
  val result: CellRW[Term] = newHole
  given Context = ctx
}

def cleanUpUnion(xs: NonEmptyVector[Term])(using ElabOps, SolverOps): NonEmptyVector[Term] = xs
  .map(toTerm(_))
  .flatMap {
    case Union(xs, _) => cleanUpUnion(xs)
    case x            => NonEmptyVector.of(x)
  }
  .distinctByEq(eqType)
  .assumeNonEmpty

case object SimplifyUnionHandler extends Handler[ElabOps, SimplifyUnion.type](SimplifyUnion) {
  override def run(c: SimplifyUnion)(using ElabOps, SolverOps): Result = {
    import c.*
    val xs = cleanUpUnion(items.map(toTerm(_)))
    val foundMeta = xs.find(_.isInstanceOf[MetaTerm[?]])
    if (foundMeta.isDefined) {
      return Result.Waiting(assumeCell(foundMeta.get))
    }
    if (xs.length == 1) {
      result.fill(xs.head)
      return Result.Done
    }
    result.fill(Union1(xs.assumeNonEmpty, meta = meta))
    Result.Done
  }

  override def canDefaulting(level: DefaultingLevel): Boolean = false
}
