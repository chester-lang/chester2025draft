package chester.erasure

import chester.error.*
import chester.syntax.core.*
import chester.tyck.*

trait Eraser {
  def checkAndErase(term: Term, ty: Term, effects: Effects)(using context: ErasureContext, reporter: Reporter[TyckProblem]): Term
}

case class ErasureContext()


object EraserImpl extends Eraser {
  def checkAndErase(term: Term, ty: Term, effects: Effects)(using context: ErasureContext, reporter: Reporter[TyckProblem]): Term = {
    term match {
      case _ => ???
    }
  }
}