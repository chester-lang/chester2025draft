package chester.tyck

import chester.syntax.*
import chester.syntax.concrete.*
import chester.error.*
import chester.resolve.{SimpleDesalt, resolveOpSeq}
import chester.utils.*

import scala.language.implicitConversions

def resolve(
    expr: Expr
)(using localCtx: Context, reporter: Reporter[TyckProblem]): Expr = {
  val result = SimpleDesalt.desugarUnwrap(expr) match {
    case opseq: OpSeq =>
      val result = resolveOpSeq(reporter, localCtx.operators, opseq)
      result
    case default => default
  }
  reuse(expr, result)
}
