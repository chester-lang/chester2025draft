package chester.elab

import chester.syntax.concrete.*
import chester.error.*
import chester.resolve.SimpleDesalt
import chester.utils.*

import scala.language.implicitConversions

def resolve(
    expr: Expr
)(using localCtx: Context, reporter: Reporter[TyckProblem]): Expr = {
  val result = SimpleDesalt.desugarUnwrap(expr)
  reuse(expr, result)
}
