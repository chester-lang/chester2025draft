package chester.elab

import cats.data.NonEmptyVector
import chester.syntax.core.*
import chester.tyck.Context
import chester.utils.elab.*
import chester.utils.assumeNonEmpty

import scala.collection.mutable

case object SimplifyUnion extends Kind {
    type Of = SimplifyUnion
  
}

case class SimplifyUnion(items: NonEmptyVector[CellROr[Term]], meta: Option[TermMeta]=None)(using ctx: Context, ops: SolverOps)
    extends Constraint(SimplifyUnion)
    with ConstraintTermRW {
  val result: CellRW[Term] = newHole
  given Context = ctx
}


def eqType(a: Term, b: Term): Boolean = (a, b) match {
  case (IntType(_), IntType(_)) => true
  case (IntegerType(_), IntegerType(_)) => true
  case (UIntType(_), UIntType(_)) => true
  case (StringType(_), StringType(_)) => true
  case (ListType(a, _), ListType(b, _)) => eqType(a, b)
  case (SymbolType(_), SymbolType(_)) => true
  case _ => false
}

case object SimplifyUnionHandler extends Handler[ElabOps, SimplifyUnion.type](SimplifyUnion) {
  override def run(c: SimplifyUnion)(using ElabOps, SolverOps): Result = {
    import c.*
    val builder = mutable.ArrayBuffer[Term]()
    for(item <- c.items.map(toTerm(_)).toVector) {
      if(!builder.exists(eqType(_, item))) builder += item
    }
    result.fill(Union(builder.toVector.assumeNonEmpty, meta = meta))
    Result.Done
  }
}
