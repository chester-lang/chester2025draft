package chester.backend.scala

import scala.meta
import chester.syntax.core
import scala.meta._
import chester.syntax.core._
object Scala {
  case class ScalaContext()
  def compileExpr(term: core.Term)(implicit ctx: ScalaContext = null): meta.Term = term match {
    case IntegerTerm(i, _) => {
      require(i.isValidInt)
      Lit.Int(i.toInt)
    }
    case IntTerm(i, _)     => Lit.Int(i)
    case StringTerm(s, _)  => Lit.String(s)
    case SymbolTerm(s, _)  => Lit.Symbol(Symbol(s))
    case BooleanTerm(b, _) => Lit.Boolean(b)
    case UnitTerm(_)       => Lit.Unit()
    case _                 => throw new NotImplementedError(s"not implemented ${term.getClass.getName} $term")
  }
  def compileTy(ty: core.Term)(implicit ctx: ScalaContext = null): meta.Type = ty match {
    case IntegerType(_) => meta.Type.Name("Int")
    case IntType(_)     => meta.Type.Name("Int")
    case StringType(_)  => meta.Type.Name("String")
    case SymbolType(_)  => meta.Type.Name("Symbol")
    case _              => throw new NotImplementedError(s"not implemented ${ty.getClass.getName} $ty")
  }
  def compileStmt(stmt: core.StmtTerm)(implicit ctx: ScalaContext = null): meta.Stat = stmt match {
    case LetStmtTerm(localv, value, ty, m) =>
      Defn.Val(
        mods = Nil,
        pats = List(meta.Pat.Var(name = meta.Term.Name(localv.name))),
        decltpe = Some(compileTy(ty)),
        rhs = compileExpr(value)
      )
    case _ => throw new NotImplementedError(s"not implemented ${stmt.getClass.getName} $stmt")
  }
}
