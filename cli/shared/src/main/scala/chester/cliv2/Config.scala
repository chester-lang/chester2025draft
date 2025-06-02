package chester.cliv2

enum Config {
  case Run(file: Option[String]) extends Config
  case Version extends Config
}