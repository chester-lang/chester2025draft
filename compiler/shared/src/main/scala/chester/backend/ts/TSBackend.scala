package chester.backend.ts

import chester.elab.*
import chester.syntax.core.*
import chester.backend.{Backend, Typescript}
import chester.uniqid.UniqidOf

import scala.collection.mutable
import scala.language.implicitConversions

implicit def metaConvert(x: Option[TermMeta]): Option[Meta] = x match {
  case Some(TermMeta(span)) => Some(Meta(Some(span)))
  case None                 => None
}

case class TSContext(map: mutable.HashMap[UniqidOf[LocalV], String])

object TSContext {
  def create(map: mutable.HashMap[UniqidOf[LocalV], String] = new mutable.HashMap()): TSContext =
    TSContext(map)
}

case object TSBackend extends Backend(Typescript) {
  def compileModule(mod: ZonkedTAST): Toplevel =
    {
      require(mod.ast.result match {
        case UnitTerm_(_) => true
        case _            => false
      }, "Module must return unit")
      given TSContext = TSContext.create()
      Toplevel(mod.ast.statements.map(compileStmt))
    }
  def compileExpr(term: Term)(using ctx: TSContext): TSExpr = term match {
    case IntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case _                    => ???
  }
  def introduceLetVar(let: LetStmtTerm)(using ctx: TSContext): String = {
    // TODO: handle shadowing and uniqueness and javascript reserved words and javascript reserved symbols and a lot more
    val name = let.localv.name
    assume(!ctx.map.contains(let.localv.uniqId), s"Variable $name already exists in the context")
    val _ = ctx.map.put(let.localv.uniqId, name)
    name
  }
  def useVar(localV: LocalV)(using ctx: TSContext): String =
    ctx.map.getOrElse(localV.uniqId, throw new IllegalArgumentException(s"Variable ${localV.name} not found in context"))
  def compileStmt(stmt: StmtTerm)(using ctx: TSContext): TSStmt = stmt match {
    case let: LetStmtTerm =>
      val name = introduceLetVar(let)
      ConstStmt(
        name = name,
        ty = Some(compileExpr(let.ty)),
        value = compileExpr(let.value),
        meta = let.meta
      )
    case _ => ???
  }
}
