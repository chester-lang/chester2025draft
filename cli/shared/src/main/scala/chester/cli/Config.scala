package chester.cli

enum Config {
  case RunConfig(input: Option[String]) extends Config
  case IntegrityConfig extends Config
  case CompileConfig(
      inputs: Seq[String],
      targetDir: String = "."
  ) extends Config
  case DecompileConfig(input: String) extends Config
  case InitConfig extends Config
  case InstallConfig extends Config
  case AddConfig(packages: Seq[String]) extends Config
  case SelfUpdateConfig extends Config
}
