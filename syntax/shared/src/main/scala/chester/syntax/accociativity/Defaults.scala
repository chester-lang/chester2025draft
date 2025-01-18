package chester.syntax.accociativity

import chester.syntax._
import upickle.default._

case class OperatorsContext(
    opinfos: InfixDefitions,
    groups: PrecedenceGroupCtx
) {
  def resolveInfix(name: Name): Option[Infix] = opinfos.resolveInfix(name)

  def resolvePrefix(name: Name): Option[Prefix] = opinfos.resolvePrefix(name)

  def resolvePostfix(name: Name): Option[Postfix] = opinfos.resolvePostfix(name)
}

object OperatorsContext {
  val Default: OperatorsContext = OperatorsContext(
    new DefaultInfixDefinitions(),
    defaultPrecedenceGroup
  )
}

trait InfixDefitions {
  def resolveInfix(name: Name): Option[Infix]

  def resolvePrefix(name: Name): Option[Prefix]

  def resolvePostfix(name: Name): Option[Postfix]

  def addOpInfo(opInfo: OpInfo): InfixDefitions
}

object InfixDefitions {
  def apply(opinfos: Vector[OpInfo]): InfixDefitions = {
    val (infix, other) = opinfos.partition {
      case Infix(_, _) => true
      case _           => false
    }
    val (prefix, other2) = other.partition {
      case Prefix(_) => true
      case _         => false
    }
    val (postfix, other3) = other2.partition {
      case Postfix(_) => true
      case _          => false
    }
    if (other3.nonEmpty) {
      throw new IllegalArgumentException(
        s"Unknown operator type: ${other3.head}"
      )
    }
    DefaultInfixDefinitions(
      infix.collect { case i @ Infix(name, _) => name -> i }.toMap,
      prefix.collect { case p @ Prefix(name) => name -> p }.toMap,
      postfix.collect { case p @ Postfix(name) => name -> p }.toMap
    )
  }
}

case class DefaultInfixDefinitions(
    infixOperators: Map[Name, Infix] = Map.empty,
    prefixOperators: Map[Name, Prefix] = Map.empty,
    postfixOperators: Map[Name, Postfix] = Map.empty
) extends InfixDefitions {

  // Map from operator symbols to their corresponding precedence groups
  private val operatorGroups: Map[Char, PrecedenceGroup] = Map(
    // Multiplicative Operators
    '*' -> multiplicativeGroup,
    '/' -> multiplicativeGroup,
    '%' -> multiplicativeGroup,

    // Additive Operators
    '+' -> additiveGroup,
    '-' -> additiveGroup,

    // Range Operator
    ':' -> rangeGroup,

    // Relational Operators
    '<' -> relationalGroup,
    '>' -> relationalGroup,

    // Equality Operators
    '=' -> equalityGroup,
    '!' -> equalityGroup,

    // Logical AND Operator
    '&' -> logicalAndGroup,

    // Logical XOR Operator
    '^' -> logicalXorGroup,

    // Logical OR Operator
    '|' -> logicalOrGroup
  )

  private val prefixes: Set[Char] = Set('!', '~', '+', '-')

  private val postfixes: Set[Char] = Set('!', '?')

  override def addOpInfo(opInfo: OpInfo): InfixDefitions = opInfo match {
    case infix: Infix =>
      if (infixOperators.contains(infix.name)) {
        throw new IllegalArgumentException(
          s"Operator ${infix.name} already defined"
        )
      }
      if (operatorGroups.contains(infix.name.head)) {
        throw new IllegalArgumentException(
          s"Operator ${infix.name} is a default operator"
        )
      }
      copy(infixOperators = infixOperators + (infix.name -> infix))
    case prefix: Prefix =>
      if (prefixOperators.contains(prefix.name)) {
        throw new IllegalArgumentException(
          s"Operator ${prefix.name} already defined"
        )
      }
      if (prefixes.contains(prefix.name.head)) {
        throw new IllegalArgumentException(
          s"Operator ${prefix.name} is a default operator"
        )
      }
      copy(prefixOperators = prefixOperators + (prefix.name -> prefix))
    case postfix: Postfix =>
      if (postfixOperators.contains(postfix.name)) {
        throw new IllegalArgumentException(
          s"Operator ${postfix.name} already defined"
        )
      }
      if (postfixes.contains(postfix.name.head)) {
        throw new IllegalArgumentException(
          s"Operator ${postfix.name} is a default operator"
        )
      }
      copy(postfixOperators = postfixOperators + (postfix.name -> postfix))
    case _ => ???
  }

  override def resolveInfix(name: Name): Option[Infix] = {
    val firstCharOption = name.headOption

    firstCharOption.flatMap { firstChar =>
      operatorGroups.get(firstChar).map { group =>
        Infix(name, group)
      }
    }
  }.orElse(infixOperators.get(name))

  override def resolvePrefix(name: Name): Option[Prefix] = {
    val firstCharOption = name.headOption
    firstCharOption
      .flatMap { firstChar =>
        if (prefixes.contains(firstChar)) {
          Some(Prefix(name))
        } else {
          None
        }
      }
      .orElse(prefixOperators.get(name))
  }

  override def resolvePostfix(name: Name): Option[Postfix] = {
    val firstCharOption = name.headOption
    firstCharOption
      .flatMap { firstChar =>
        if (postfixes.contains(firstChar)) {
          Some(Postfix(name))
        } else {
          None
        }
      }
      .orElse(postfixOperators.get(name))
  }
}

val defaultPrecedenceGroup = PrecedenceGroupCtx(
  Map(
    Names.Multiplicative.name -> multiplicativeGroup,
    Names.Additive.name -> additiveGroup,
    Names.Range.name -> rangeGroup,
    Names.Relational.name -> relationalGroup,
    Names.Equality.name -> equalityGroup,
    Names.LogicalAnd.name -> logicalAndGroup,
    Names.LogicalXor.name -> logicalXorGroup,
    Names.LogicalOr.name -> logicalOrGroup
  )
)

case class PrecedenceGroupCtx(precedenceGroups: Map[Name, PrecedenceGroup]) {
  def groups: Seq[PrecedenceGroup] = precedenceGroups.values.toVector
}

object Names {
  val Multiplicative: QualifiedIDString = QualifiedIDString.from("Multiplicative")
  val Additive: QualifiedIDString = QualifiedIDString.from("Additive")
  val Range: QualifiedIDString = QualifiedIDString.from("Range")
  val Relational: QualifiedIDString = QualifiedIDString.from("Relational")
  val Equality: QualifiedIDString = QualifiedIDString.from("Equality")
  val LogicalAnd: QualifiedIDString = QualifiedIDString.from("LogicalAnd")
  val LogicalXor: QualifiedIDString = QualifiedIDString.from("LogicalXor")
  val LogicalOr: QualifiedIDString = QualifiedIDString.from("LogicalOr")
  val Default: QualifiedIDString = QualifiedIDString.from("Default")
}

lazy val DefaultPrecedenceGroup: PrecedenceGroup = PrecedenceGroup(
  Names.Default
)

// Define the precedence groups with their associativity and precedence
lazy val logicalOrGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.LogicalOr,
  associativity = Associativity.Left,
  higherThan = Vector(DefaultPrecedenceGroup)
)

lazy val logicalXorGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.LogicalXor,
  associativity = Associativity.Left,
  higherThan = Vector(logicalOrGroup)
)

lazy val logicalAndGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.LogicalAnd,
  associativity = Associativity.Left,
  higherThan = Vector(logicalXorGroup)
)

lazy val equalityGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.Equality,
  associativity = Associativity.Chain,
  higherThan = Vector(logicalAndGroup)
)

lazy val relationalGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.Relational,
  associativity = Associativity.Chain,
  higherThan = Vector(equalityGroup)
)

lazy val rangeGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.Range,
  associativity = Associativity.None,
  higherThan = Vector(relationalGroup)
)

lazy val additiveGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.Additive,
  associativity = Associativity.Left,
  higherThan = Vector(rangeGroup)
)

lazy val multiplicativeGroup: PrecedenceGroup = PrecedenceGroup(
  name = Names.Multiplicative,
  associativity = Associativity.Left,
  higherThan = Vector(additiveGroup)
)
