package chester.targets

sealed trait Target {
  def name: String
  type ModuleType
}

case object Typescript extends Target {
  val name: String = "ts"
}

case object Scala extends Target {
  val name: String = "scala"
}

case object Rust extends Target {
  val name: String = "rust"
}

trait Backend[AST](val target: Target) {}
