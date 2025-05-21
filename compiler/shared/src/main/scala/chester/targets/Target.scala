package chester.targets

enum Target(val name: String) {
  case Typescript extends Target("ts")
  case Scala extends Target("scala")
  case Rust extends Target("rust")
}

trait Backend[AST](val target: Target) {}
