package chester.utils.env

def isTermux: Boolean = {
  // Define the string to check for
  val termuxString = "/data/data/com.termux"

  // Check each environment variable for the presence of the termuxString
  val prefixIncludesTermux =
    sys.env.get("PREFIX").exists(_.contains(termuxString))
  val ldPreloadIncludesTermux =
    sys.env.get("LD_PRELOAD").exists(_.contains(termuxString))
  val shellIncludesTermux =
    sys.env.get("SHELL").exists(_.contains(termuxString))
  val pathIncludesTermux = sys.env.get("PATH").exists(_.contains(termuxString))

  // Return true if any of the checks pass
  prefixIncludesTermux || ldPreloadIncludesTermux || shellIncludesTermux || pathIncludesTermux
}
val platform$getOS: OS = {
  val os = System.getProperty("os.name").toLowerCase
  if (os.contains("win")) OS.Windows
  else if (os.contains("mac")) OS.Mac
  else if (os.contains("linux")) (if (isTermux) OS.Termux else OS.GNULinux)
  else if (os.contains("freebsd")) OS.FreeBSD
  else if (os.contains("openbsd")) OS.OpenBSD
  else if (os.contains("netbsd")) OS.NetBSD
  else throw new Exception(s"Unknown OS: $os")
}
val platform$getArch: Architecture = {
  System.getProperty("os.arch").toLowerCase match {
    case "x86_64" | "amd64" | "x64" => Architecture.Amd64
    case "x86" | "i386"             => Architecture.X86
    case "arm"                      => Architecture.Arm
    case "aarch64" | "arm64"        => Architecture.Arm64
    case _ =>
      throw new Exception(
        s"Unknown architecture: ${System.getProperty("os.arch")}"
      )
  }
}
