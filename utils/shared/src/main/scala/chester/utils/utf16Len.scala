package chester.utils

import spire.math.Natural

extension (text: String) {
  def utf16Len: Natural = Nat(text.length)
  // TODO: optimize
  def unicodeLen: Natural = Nat(text.getCodePoints.length)
  def lenIsOne: Boolean =
    text.length == 1 || (text.length == 2 && Character.isHighSurrogate(text.charAt(0)) && Character.isLowSurrogate(text.charAt(1)))
}
