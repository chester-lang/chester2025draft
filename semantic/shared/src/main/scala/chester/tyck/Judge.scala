package chester.tyck

import chester.syntax.core.*

/** A type judgement, containing both a term and its type */
case class Judge(term: Term, ty: Term)
