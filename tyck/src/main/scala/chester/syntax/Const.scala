package chester.syntax

object Const {
  val Record = "record"
  val Case: String = "case"
  val Data: String = "data"
  val Trait: String = "trait"
  val Implement: String = "implement"
  val Import: String = "import"
  val Module: String = "module"
  val Arrow2: String = "=>"
  val Arrow: String = "->"
  val Let: String = "let"
  val Def: String = "def"
  val `:`: String = ":"
  val `=`: String = "="
  val kw1: Set[String] = Set(Let, Def)
  val `<:`: String = "<:"
  val Interface = "interface"
  val `with` = "with"
  val Object = "object"
}
