package chester.elab

import chester.syntax.core.Term
import chester.utils.elab.*
import spire.math.Trilean

extension (sub: CellRWOr[Term]) {
  def `<:?`(supertype: CellRWOr[Term]): Trilean = ???
}

