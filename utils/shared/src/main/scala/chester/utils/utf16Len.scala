package chester.utils

import spire.math.Natural

extension (s: String) {
  def utf16Len: Natural = Nat(s.length)
}