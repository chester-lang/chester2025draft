package chester.targets.ts

import chester.elab.*
import chester.syntax.core.*
import chester.targets.{Backend, Typescript}
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
    ???
  def compileExpr(term: Term)(using ctx: TSContext): TSExpr = term match {
    case IntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case _                    => ???
  }
  def compileStmt(stmt: StmtTerm)(using ctx: TSContext): TSStmt = stmt match {
    case _ => ???
  }
}
