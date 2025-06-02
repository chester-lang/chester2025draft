package chester.targets.ts

import chester.elab.*
import chester.syntax.core.*
import chester.targets.{Backend, Typescript}
import scala.language.implicitConversions

// TODO
implicit def metaConvert(x: Option[TermMeta]): Option[Meta] = None

case object TSBackend extends Backend(Typescript) {
  def compileModule(mod: ZonkedTAST): Toplevel =
    ???
  def compileExpr(term: Term): TSExpr = term match {
    case IntTerm(value, meta) => DoubleExpr(value.toDouble, meta)
    case _                    => ???
  }
}
