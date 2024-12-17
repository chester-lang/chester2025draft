package chester.syntax.accociativity

import chester.syntax.{Name, QualifiedIDString}
import upickle.default.*

case class PrecedenceGroup(
    name: QualifiedIDString,
    higherThan: Vector[PrecedenceGroup] = Vector(),
    lowerThan: Vector[PrecedenceGroup] = Vector(),
    associativity: Associativity = Associativity.None
) derives ReadWriter

enum Associativity derives ReadWriter {
  case None
  case Left
  case Right
  case Chain // TODO
}

sealed trait OpInfo {
  def name: Name
}

sealed trait OpWithGroup extends OpInfo {
  def group: PrecedenceGroup
}

sealed trait OpUnary extends OpInfo

case class Prefix(name: Name) extends OpUnary

case class Postfix(name: Name) extends OpUnary

case class Infix(
    name: Name,
    group: PrecedenceGroup = DefaultPrecedenceGroup
) extends OpWithGroup

case class Mixfix(
    names: Vector[Name],
    group: PrecedenceGroup = DefaultPrecedenceGroup
) extends OpWithGroup {
  def name: Name = ??? // Implement as needed
}
