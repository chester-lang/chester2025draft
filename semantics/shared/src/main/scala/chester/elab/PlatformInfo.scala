package chester.elab

import chester.utils.Parameter
import spire.math.UInt

enum NativeCharIndexInString {
  case Byte, UTF16, UTF32
}

trait PlatformInfo {
  def charIndexInString: NativeCharIndexInString
  def verify(): Unit = {

    // requires at least 32 bits for Int
    require(IntMin <= Int.MinValue.toLong, s"IntMin must be less than or equal to Int.MinValue, but got $IntMin vs ${Int.MinValue.toLong}")

    require(IntMax >= Int.MaxValue.toLong)
    require(UIntMax >= IntMax)

    if (!hasUInt)
      require(UIntMax == IntMax, s"UIntMax must be equal to IntMax when hasUInt is false, but got UIntMax = $UIntMax and IntMax = $IntMax")
  }
  // 128 bis platform, for example, is not supported currently
  def IntMin: Long
  def IntMax: Long
  def hasUInt: Boolean
  final def UIntMin: Long = 0L
  def UIntMax: BigInt = IntMax
  def isValidInt(value: BigInt): Boolean =
    value >= IntMin && value <= IntMax
  def isValidUInt(value: BigInt): Boolean =
    value >= UIntMin && value <= UIntMax
  def isValidInt(value: Long): Boolean =
    value >= IntMin && value <= IntMax
}

object JVMPlatformInfo extends PlatformInfo {
  override val IntMin: Long = Int.MinValue.toLong
  override val IntMax: Long = Int.MaxValue.toLong
  override val UIntMax: BigInt = UInt.MaxValue.toBigInt
  override val hasUInt: Boolean = true
  override def charIndexInString: NativeCharIndexInString = NativeCharIndexInString.UTF16
  verify()
}

object TypescriptPlatformInfo extends PlatformInfo {
  override val IntMin: Long = -9007199254740991L // -(2^53 - 1)
  override val IntMax: Long = 9007199254740991L // 2^53 - 1
  override val hasUInt: Boolean = false
  override def charIndexInString: NativeCharIndexInString = NativeCharIndexInString.UTF16
  verify()
}

val platformInfo: Parameter[PlatformInfo] = new Parameter[PlatformInfo]()

def PlatformInfo: PlatformInfo =
  platformInfo.getOrElse {
    throw new IllegalStateException("PlatformInfo is not set. Please set it before using.")
  }
