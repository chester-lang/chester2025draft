package chester.elab

import chester.utils.Parameter
import spire.math.UInt

trait PlatformInfo {
  // 128 bis platform, for example, is not supported currently
  def IntMin: Long
  def IntMax: Long
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
}

object TypescriptPlatformInfo extends PlatformInfo {
  override val IntMin: Long = -9007199254740991L // -(2^53 - 1)
  override val IntMax: Long = 9007199254740991L // 2^53 - 1
}

val platformInfo: Parameter[PlatformInfo] = new Parameter[PlatformInfo]()

def PlatformInfo: PlatformInfo =
  platformInfo.getOrElse {
    throw new IllegalStateException("PlatformInfo is not set. Please set it before using.")
  }
