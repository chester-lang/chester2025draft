package chester.targets

import chester.targets.ts.Toplevel

sealed trait Target {
  def name: String
  type ModuleType
}

case object Typescript extends Target {
  override val name: String = "ts"
  override type ModuleType = Toplevel
}

case object Scala extends Target {
  val name: String = "scala"
}

case object Rust extends Target {
  val name: String = "rust"
}

trait Backend(val target: Target) {}
