package chester.erasure

import chester.error._
import chester.syntax.core._
import chester.tyck._

trait Eraser {
  def checkAndErase(term: Term, ty: Term, effects: Effects)(using context: ErasureContext, reporter: Reporter[TyckProblem]): Term
}

case class ErasureContext()

object EraserImpl extends Eraser {
  def checkAndErase(term: Term, ty: Term, effects: Effects)(using  ErasureContext,  Reporter[TyckProblem]): Term = {
    term match {
      case _ => ???
    }
  }
}
