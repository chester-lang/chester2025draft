package chester.syntax.core.orm

import chester.syntax.core.*
import upickle.default.*

import scala.language.implicitConversions
import scala.reflect.ClassTag

/** note that this disallow non normal form terms, so avoid using it when non normal form terms are allowed
  *
  * OrM = Or MetaTerm
  */
type OrM[T <: Term] = T | MetaTerm[Term]
type OrM1[T <: Term] = T | MetaTerm[T]

// this needs to be in a separate package to avoid circular runtime dependencies which results in dead lock as given is implemented with lazy val and circular lazy val will cause deadloop
implicit def OrMRW1[T <: Term](using rw: ReadWriter[Term], ct: ClassTag[T]): ReadWriter[OrM1[T]] =
  rw.bimap(
    (term: OrM1[T]) =>
      term match {
        case m: MetaTerm[T @unchecked] => m
        case t: T                      => t
      },
    (term: Term) =>
      term match {
        case b: T                      => b
        case m: MetaTerm[T @unchecked] => m
        case _                         => throw new IllegalArgumentException(s"Expected $ct or MetaTerm[?], but got $term")
      }
  )

// this needs to be in a separate package to avoid circular runtime dependencies which results in dead lock as given is implemented with lazy val and circular lazy val will cause deadloop
given OrMRW[T <: Term](using rw: ReadWriter[Term], ct: ClassTag[T]): ReadWriter[OrM[T]] =
  rw.bimap(
    (term: OrM[T]) =>
      term match {
        case m: MetaTerm[Term @unchecked] => m
        case t: T                         => t
      },
    (term: Term) =>
      term match {
        case b: T                         => b
        case m: MetaTerm[Term @unchecked] => m
        case _                            => throw new IllegalArgumentException(s"Expected $ct or MetaTerm[?], but got $term")
      }
  )
