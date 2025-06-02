package chester.targets.ts

import chester.elab.*
import chester.syntax.core.*
import chester.targets.{Backend, Typescript}
import scala.language.implicitConversions

implicit def metaConvert(x: Option[TermMeta]): Option[Meta] = x match {
  case Some(TermMeta(span)) => Some(Meta(Some(span)))
  case None                 => None
}

case object TSBackend extends Backend(Typescript) {
  def compileModule(mod: ZonkedTAST): Toplevel =
    ???
  def compileExpr(term: Term): TSExpr = term match {
    case IntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case _                    => ???
  }
  def compileStmt(stmt: StmtTerm): TSStmt = stmt match {
    case _ => ???
  }
}
