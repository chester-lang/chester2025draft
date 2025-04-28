package chester.utils

import upickle.default.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import _root_.io.github.iltotore.iron.upickle.given
import spire.math.UInt
import chester.utils.impls.uintRW

// i is unicode character position
case class WithUTF16(i: spire.math.UInt, utf16: spire.math.UInt) derives ReadWriter {
  require(i <= utf16, "i must be less than or equal to utf16")
  def <(other: WithUTF16): Boolean = i < other.i && utf16 < other.utf16
  def >(other: WithUTF16): Boolean = i > other.i && utf16 > other.utf16
  def <=(other: WithUTF16): Boolean = i <= other.i && utf16 <= other.utf16
  def >=(other: WithUTF16): Boolean = i >= other.i && utf16 >= other.utf16
  def +(other: WithUTF16): WithUTF16 =
    WithUTF16(i + other.i, utf16 + other.utf16)
  def isZero: Boolean = i == UInt(0) && utf16 == UInt(0)
  def nonZero: Boolean = i != UInt(0) && utf16 != UInt(0)
}

object WithUTF16 {
  val Zero: WithUTF16 = WithUTF16(UInt(0), UInt(0))
  val One: WithUTF16 = WithUTF16(UInt(1), UInt(1))
}
