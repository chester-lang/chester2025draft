package chester.cli

enum Config {
  case Run(file: Option[String]) extends Config
  case Version extends Config
  case Compile(target: Option[String], inputFile: String, outputFile: Option[String]) extends Config
}
