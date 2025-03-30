package chester.utils.env

import typings.node.processMod.global.NodeJS.Platform
import typings.node.{osMod, processMod}

val platform$getOS: OS = {
  val os = osMod.platform()
  if (os == Platform.win32) OS.Windows
  else if (os == Platform.darwin) OS.Mac
  else if (os == Platform.linux) OS.GNULinux
  else if (os == Platform.android) OS.Termux
  else if (os == Platform.freebsd) OS.FreeBSD
  else if (os == Platform.openbsd) OS.OpenBSD
  else if (os == Platform.netbsd) OS.NetBSD
  else OS.Other
}
val platform$getArch: Architecture =
  osMod.arch().toLowerCase match {
    case "x86_64" | "amd64" | "x64" => Architecture.Amd64
    case "x86" | "i386"             => Architecture.X86
    case "arm"                      => Architecture.Arm
    case "aarch64" | "arm64"        => Architecture.Arm64
    case _                          => Architecture.Other
  }
val platform$getRunningOn: RunningOn = RunningOn.Nodejs(processMod.^.version)
