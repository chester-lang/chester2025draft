package chester.tyck

import chester.syntax.concrete.ExprMeta
import chester.syntax.core.*

def convertMeta(meta: Option[ExprMeta]): Option[TermMeta] = for {
  m <- meta
  s <- m.span
} yield TermMeta(s)
