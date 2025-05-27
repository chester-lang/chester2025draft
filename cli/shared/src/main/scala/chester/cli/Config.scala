package chester.cli

enum Config {
  case RunConfig(input: Option[String]) extends Config
  case IntegrityConfig extends Config
  @deprecated("internal testing only")
  case Compile1Config(
      inputPath: String,
      targetName: String,
      outputPath: String
  )
  @deprecated("needs rework")
  case CompileConfig(
      inputs: Seq[String],
      targetDir: String = ".",
      tastDirs: Seq[String] = Seq()
  ) extends Config
  @deprecated("needs rework")
  case DecompileConfig(input: String) extends Config
  @deprecated("needs rework")
  case InitConfig extends Config
  @deprecated("needs rework")
  case InstallConfig extends Config
  @deprecated("needs rework")
  case AddConfig(packages: Seq[String]) extends Config
  case SelfUpdateConfig extends Config
  @deprecated("needs rework")
  case FormatConfig(files: Seq[String]) extends Config // Added line
}
