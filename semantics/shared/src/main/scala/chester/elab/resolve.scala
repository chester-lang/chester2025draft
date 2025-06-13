package chester.elab

import chester.syntax.concrete.*
import chester.error.*
import chester.resolve.SimpleDesalt
import chester.utils.*

import scala.language.implicitConversions

def resolve(
    expr: Expr
)(using Context, Reporter[TyckProblem]): Expr = {
  val result = SimpleDesalt.desugarUnwrap(expr)
  reuse(expr, result)
}

def resolveTele(expr: Expr)(using Context, Reporter[TyckProblem]): Option[Expr] = {
  val result = SimpleDesalt.desugarTele(expr)
  result.map(result => reuse(expr, result))
}

object ResolveTele {
  def unapply(expr: Expr)(using Context, Reporter[TyckProblem]): Option[Expr] =
    SimpleDesalt.desugarTele(expr)
}
