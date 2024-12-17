package chester.syntax.core.orm

import chester.syntax.core.*
import upickle.default.*

import scala.language.implicitConversions

/** note that this disallow non normal form terms, so avoid using it when non normal form terms are allowed */
type OrM[T <: Term] = (T | MetaTerm)
/*
// This type can't be used if scala2.13 interoperability is required
type EffectsM = OrM[Effects]

object EffectsM {
  def is(x: Any): Boolean = x match {
    case Effects(_, _)  => true
    case MetaTerm(_, _) => true
    case _              => false
  }
}
 */
given OrMRW[T <: Term](using rw: ReadWriter[Term]): ReadWriter[OrM[T]] =
  rw.asInstanceOf[ReadWriter[OrM[T]]]

extension (e: EffectsM) {
  def nonEmpty: Boolean = e match {
    case e: Effects => e.nonEmpty
    case _          => true
  }
}
