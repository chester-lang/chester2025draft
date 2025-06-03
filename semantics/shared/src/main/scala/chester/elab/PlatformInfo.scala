package chester.elab

trait PlatformInfo {
  def IntMin: Long
  def IntMax: Long
  def isValidInt(value: Long): Boolean =
    value >= IntMin && value <= IntMax
}

object JVMPlatformInfo extends PlatformInfo {
  override def IntMin: Long = Int.MinValue.toLong
  override def IntMax: Long = Int.MaxValue.toLong
}

object JSPlatformInfo extends PlatformInfo {
  override def IntMin: Long = -9007199254740991L // -(2^53 - 1)
  override def IntMax: Long = 9007199254740991L // 2^53 - 1
}
