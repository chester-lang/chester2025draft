package chester.elab

import scala.language.implicitConversions
import chester.syntax.concrete.ExprMeta
import chester.syntax.core.*

implicit def convertMeta(meta: Option[ExprMeta]): Option[TermMeta] = for {
  m <- meta
  s <- m.span
} yield TermMeta(s)
