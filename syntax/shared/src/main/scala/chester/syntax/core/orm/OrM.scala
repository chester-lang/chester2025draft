package chester.syntax.core.orm

import chester.syntax.core.*
import upickle.default.*

import scala.language.implicitConversions

/** note that this disallow non normal form terms, so avoid using it when non normal form terms are allowed */
type OrM[T <: Term] = T | MetaTerm

given OrMRW[T <: Term](using rw: ReadWriter[Term]): ReadWriter[OrM[T]] =
  rw.asInstanceOf[ReadWriter[OrM[T]]]
