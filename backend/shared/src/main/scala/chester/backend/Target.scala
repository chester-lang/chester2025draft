package chester.backend

import chester.backend.ts.Toplevel

sealed trait Target extends Product with Serializable {
  def name: String
  type ModuleType
}

case object Typescript extends Target {
  override val name: String = "typescript"
  override type ModuleType = Toplevel
}

case object Scala extends Target {
  val name: String = "scala"
}

case object Rust extends Target {
  val name: String = "rust"
}

trait Backend(val target: Target) {}
