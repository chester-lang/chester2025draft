package chester.utils.env

val isUNIX = platform$getOS.isUNIX
val isWindows = platform$getOS == OS.Windows

implicit object DefaultEnv extends Environment {
  override def getOS: OS = platform$getOS
  override def getArch: Architecture = platform$getArch
  override def getRunningOn: RunningOn = platform$getRunningOn
  override def hasWindowsNarrator: Boolean = WindowsNarratorChecker()
}
