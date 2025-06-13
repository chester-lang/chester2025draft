package chester.elab

import chester.syntax.concrete.*
import chester.error.*
import chester.resolve.{DeclTeleMode, SimpleDesalt}
import chester.utils.*

import scala.language.implicitConversions

def resolve(
    expr: Expr
)(using Context, Reporter[TyckProblem]): Expr = {
  val result = SimpleDesalt.desugarUnwrap(expr)
  reuse(expr, result)
}

def resolveTele(expr: Expr)(using mode: DeclTeleMode = DeclTeleMode.Default, ctx: Context, reporter: Reporter[TyckProblem]): Option[DefTelescope] = {
  val result = SimpleDesalt.desugarTele(expr)
  result.map(result => reuse(expr, result))
}

object ResolveTele {
  def unapply(expr: Expr)(using mode: DeclTeleMode = DeclTeleMode.Default, ctx: Context, reporter: Reporter[TyckProblem]): Option[DefTelescope] =
    resolveTele(expr)
}

object ResolveTeleType {
  def unapply(expr: Expr)(using ctx: Context, reporter: Reporter[TyckProblem]): Option[DefTelescope] = {
    given mode: DeclTeleMode = DeclTeleMode.Type
    resolveTele(expr)
  }

}
