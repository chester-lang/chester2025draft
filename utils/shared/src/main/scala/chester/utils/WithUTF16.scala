package chester.utils

import upickle.default.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import _root_.io.github.iltotore.iron.upickle.given

case class WithUTF16(i: Int :| Positive0, utf16: Int :| Positive0) derives ReadWriter {
  require(i <= utf16, "i must be less than or equal to utf16")
  def <(other: WithUTF16): Boolean = i < other.i && utf16 < other.utf16
  def >(other: WithUTF16): Boolean = i > other.i && utf16 > other.utf16
  def <=(other: WithUTF16): Boolean = i <= other.i && utf16 <= other.utf16
  def >=(other: WithUTF16): Boolean = i >= other.i && utf16 >= other.utf16
  def +(other: WithUTF16): WithUTF16 =
    WithUTF16((i + other.i).refineUnsafe, (utf16 + other.utf16).refineUnsafe)
  def isZero: Boolean = i == 0 && utf16 == 0
  def nonZero: Boolean = i != 0 && utf16 != 0
}

object WithUTF16 {
  val Zero: WithUTF16 = WithUTF16(0, 0)
  val One: WithUTF16 = WithUTF16(1, 1)
}
