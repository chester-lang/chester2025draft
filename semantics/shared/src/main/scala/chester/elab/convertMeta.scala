package chester.elab

import chester.syntax.concrete.ExprMeta
import chester.syntax.core.*

import scala.language.implicitConversions

implicit def convertMeta(meta: Option[ExprMeta]): Option[TermMeta] = for {
  m <- meta
  s <- m.span
} yield TermMeta(s)
