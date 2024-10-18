package chester.targets.js

// Base trait for all AST nodes
sealed trait ASTNode

// Expressions
sealed trait Expression extends ASTNode

case class Identifier(
  name: String,
  typeAnnotation: Option[TypeAnnotation] = None
) extends Expression

// Literals
sealed trait Literal extends Expression

case class NumericLiteral(value: Double) extends Literal

case class StringLiteral(value: String) extends Literal

case class BooleanLiteral(value: Boolean) extends Literal

case object NullLiteral extends Literal

case class BigIntLiteral(value: String) extends Literal // BigInt as String to preserve precision

case class RegExpLiteral(pattern: String, flags: String) extends Literal

// Template Literals
case class TemplateLiteral(
  quasis: List[TemplateElement],
  expressions: List[Expression]
) extends Expression

case class TemplateElement(
  value: String,
  tail: Boolean
) extends ASTNode

// Expressions
case class BinaryExpression(
  operator: String,
  left: Expression,
  right: Expression
) extends Expression

case class AssignmentExpression(
  operator: String, // E.g., "=", "+=", "-=", etc.
  left: Expression, // Could be Identifier or MemberExpression
  right: Expression
) extends Expression

case class LogicalExpression(
  operator: String, // "||", "&&", "??"
  left: Expression,
  right: Expression
) extends Expression

case class UnaryExpression(
  operator: String, // "-", "+", "!", "~", "typeof", "void", "delete"
  argument: Expression,
  prefix: Boolean = true
) extends Expression

case class UpdateExpression(
  operator: String, // "++", "--"
  argument: Expression,
  prefix: Boolean
) extends Expression

case class ConditionalExpression(
  test: Expression,
  consequent: Expression,
  alternate: Expression
) extends Expression

case class CallExpression(
  callee: Expression,
  arguments: List[Expression],
  optional: Boolean = false // For optional chaining
) extends Expression

case class NewExpression(
  callee: Expression,
  arguments: List[Expression]
) extends Expression

case class MemberExpression(
  obj: Expression,
  property: Expression,
  computed: Boolean,
  optional: Boolean = false // For optional chaining
) extends Expression

case class OptionalMemberExpression(
  obj: Expression,
  property: Expression,
  computed: Boolean,
  optional: Boolean = true
) extends Expression

case class FunctionExpression(
  id: Option[Identifier],
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  async: Boolean = false,
  generator: Boolean = false
) extends Expression

case class ArrowFunctionExpression(
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: Either[Expression, BlockStatement],
  async: Boolean = false
) extends Expression

case class ClassExpression(
  id: Option[Identifier],
  typeParameters: Option[List[TypeParameter]] = None,
  superClass: Option[Expression] = None,
  implementsInterfaces: Option[List[Expression]] = None,
  body: ClassBody,
  decorators: Option[List[Decorator]] = None
) extends Expression

case class AwaitExpression(argument: Expression) extends Expression

case class YieldExpression(
  argument: Option[Expression],
  delegate: Boolean = false
) extends Expression

case class TaggedTemplateExpression(
  tag: Expression,
  quasi: TemplateLiteral
) extends Expression

case class SequenceExpression(expressions: List[Expression]) extends Expression

case class SpreadElement(argument: Expression) extends Expression

case class ObjectExpression(
  properties: List[ObjectProperty]
) extends Expression

case class ObjectProperty(
  key: Expression,
  value: Expression,
  computed: Boolean = false,
  shorthand: Boolean = false,
  method: Boolean = false
) extends ASTNode

case class ArrayExpression(
  elements: List[Option[Expression]]
) extends Expression

// Patterns for destructuring
sealed trait Pattern extends ASTNode

case class ObjectPattern(
  properties: List[PatternProperty],
  typeAnnotation: Option[TypeAnnotation] = None
) extends Pattern

case class ArrayPattern(
  elements: List[Option[Pattern]],
  typeAnnotation: Option[TypeAnnotation] = None
) extends Pattern

case class PatternProperty(
  key: Expression,
  value: Pattern,
  computed: Boolean = false,
  shorthand: Boolean = false
) extends ASTNode

case class RestElement(
  argument: Pattern
) extends Pattern

case class AssignmentPattern(
  left: Pattern,
  right: Expression
) extends Pattern

// Statements
sealed trait Statement extends ASTNode

sealed trait Declaration extends Statement

case class VariableDeclaration(
  kind: VariableKind,
  declarations: List[VariableDeclarator]
) extends Declaration

// VariableKind enumeration
sealed trait VariableKind
object VariableKind {
  case object Var extends VariableKind
  case object Let extends VariableKind
  case object Const extends VariableKind
}

case class VariableDeclarator(
  id: Pattern, // Using Pattern for destructuring
  init: Option[Expression]
) extends ASTNode

case class FunctionDeclaration(
  id: Identifier,
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  async: Boolean = false,
  generator: Boolean = false,
  typeParameters: Option[List[TypeParameter]] = None
) extends Declaration

case class Parameter(
  id: Pattern
) extends ASTNode

case class BlockStatement(
  body: List[Statement]
) extends Statement

case class ExpressionStatement(
  expression: Expression
) extends Statement

// Control Flow Statements
case class IfStatement(
  test: Expression,
  consequent: Statement,
  alternate: Option[Statement] = None
) extends Statement

case class ForStatement(
  init: Option[Either[VariableDeclaration, Expression]],
  test: Option[Expression],
  update: Option[Expression],
  body: Statement
) extends Statement

case class WhileStatement(
  test: Expression,
  body: Statement
) extends Statement

case class DoWhileStatement(
  body: Statement,
  test: Expression
) extends Statement

case class ForInStatement(
  left: Either[VariableDeclaration, Expression],
  right: Expression,
  body: Statement
) extends Statement

case class ForOfStatement(
  left: Either[VariableDeclaration, Expression],
  right: Expression,
  body: Statement,
  await: Boolean = false
) extends Statement

case class SwitchStatement(
  discriminant: Expression,
  cases: List[SwitchCase]
) extends Statement

case class SwitchCase(
  test: Option[Expression], // None for default case
  consequent: List[Statement]
) extends ASTNode

case class BreakStatement(
  label: Option[Identifier] = None
) extends Statement

case class ContinueStatement(
  label: Option[Identifier] = None
) extends Statement

case class ReturnStatement(
  argument: Option[Expression]
) extends Statement

// Try/Catch/Finally Statements
case class TryStatement(
  block: BlockStatement,
  handler: Option[CatchClause],
  finalizer: Option[BlockStatement]
) extends Statement

case class CatchClause(
  param: Option[Pattern],
  body: BlockStatement
) extends ASTNode

case class ThrowStatement(
  argument: Expression
) extends Statement

// Classes
case class ClassDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  superClass: Option[Expression] = None,
  implementsInterfaces: Option[List[Expression]] = None,
  body: ClassBody,
  decorators: Option[List[Decorator]] = None
) extends Declaration

case class ClassBody(
  body: List[ClassElement]
) extends ASTNode

sealed trait ClassElement extends ASTNode

case class MethodDefinition(
  key: Expression,
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  kind: MethodKind, // "constructor", "method", "get", "set"
  isStatic: Boolean = false,
  computed: Boolean = false,
  decorators: Option[List[Decorator]] = None
) extends ClassElement

// MethodKind enumeration
sealed trait MethodKind
object MethodKind {
  case object Constructor extends MethodKind
  case object Method extends MethodKind
  case object Get extends MethodKind
  case object Set extends MethodKind
}

case class PropertyDefinition(
  key: Expression,
  value: Option[Expression] = None,
  isStatic: Boolean = false,
  computed: Boolean = false,
  decorators: Option[List[Decorator]] = None
) extends ClassElement

case class StaticBlock(
  body: List[Statement]
) extends ClassElement

// Modules: Import and Export Declarations
case class ImportDeclaration(
  specifiers: List[ImportSpecifier],
  source: StringLiteral
) extends Statement

sealed trait ImportSpecifier extends ASTNode

case class ImportDefaultSpecifier(
  local: Identifier
) extends ImportSpecifier

case class ImportNamespaceSpecifier(
  local: Identifier
) extends ImportSpecifier

case class ImportNamedSpecifier(
  local: Identifier,
  imported: Identifier
) extends ImportSpecifier

sealed trait ExportDeclaration extends Statement

case class ExportNamedDeclaration(
  declaration: Option[Declaration],
  specifiers: List[ExportSpecifier],
  source: Option[StringLiteral]
) extends ExportDeclaration

case class ExportDefaultDeclaration(
  declaration: Declaration
) extends ExportDeclaration

case class ExportAllDeclaration(
  source: StringLiteral,
  exported: Option[Identifier] = None
) extends ExportDeclaration

case class ExportSpecifier(
  local: Identifier,
  exported: Identifier
) extends ASTNode

// TypeScript-specific nodes
sealed trait TypeAnnotation extends ASTNode

case class TypeReference(
  name: String,
  typeParameters: Option[List[TypeAnnotation]] = None
) extends TypeAnnotation

case class FunctionTypeAnnotation(
  params: List[Parameter],
  returnType: TypeAnnotation
) extends TypeAnnotation

// Union and Intersection Types
case class UnionType(
  types: List[TypeAnnotation]
) extends TypeAnnotation

case class IntersectionType(
  types: List[TypeAnnotation]
) extends TypeAnnotation

// Generics
case class GenericTypeAnnotation(
  id: Identifier,
  typeParameters: Option[List[TypeAnnotation]] = None
) extends TypeAnnotation

// Primitive Type Annotations
case object AnyTypeAnnotation extends TypeAnnotation

case object UnknownTypeAnnotation extends TypeAnnotation

case object NumberTypeAnnotation extends TypeAnnotation

case object StringTypeAnnotation extends TypeAnnotation

case object BooleanTypeAnnotation extends TypeAnnotation

case object NullTypeAnnotation extends TypeAnnotation

case object UndefinedTypeAnnotation extends TypeAnnotation

case object NeverTypeAnnotation extends TypeAnnotation

case object ObjectTypeAnnotation extends TypeAnnotation

case object SymbolTypeAnnotation extends TypeAnnotation

// Type Parameters
case class TypeParameter(
  name: String,
  constraint: Option[TypeAnnotation] = None,
  default: Option[TypeAnnotation] = None
) extends ASTNode

// Enums and Type Aliases (TypeScript)
case class EnumDeclaration(
  id: Identifier,
  members: List[EnumMember]
) extends Declaration

case class EnumMember(
  id: Identifier,
  initializer: Option[Expression] = None
) extends ASTNode

case class TypeAliasDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  typeAnnotation: TypeAnnotation
) extends Declaration

// Type Assertions and As Expressions (TypeScript)
case class TypeAssertion(
  typeAnnotation: TypeAnnotation,
  expression: Expression
) extends Expression

case class AsExpression(
  expression: Expression,
  typeAnnotation: TypeAnnotation
) extends Expression

case class NonNullExpression(
  expression: Expression
) extends Expression

// Decorators (TypeScript)
case class Decorator(
  expression: Expression
) extends ASTNode

// Destructuring and Spread Elements
// Already handled in Patterns and SpreadElement above

// Generators and Yield Expressions
// YieldExpression defined above

// Async Generators
case class AsyncGeneratorFunctionDeclaration(
  id: Option[Identifier],
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  typeParameters: Option[List[TypeParameter]] = None
) extends Declaration

// Index Signatures and Mapped Types (TypeScript)
case class IndexSignature(
  parameter: Parameter,
  typeAnnotation: TypeAnnotation
) extends ASTNode

case class MappedType(
  typeParameter: TypeParameter,
  typeAnnotation: TypeAnnotation,
  optional: Boolean = false,
  readonly: Boolean = false
) extends TypeAnnotation

// Symbol Type and Symbol Expressions
case class SymbolExpression(
  description: Option[Expression]
) extends Expression

// Await and Async Functions already handled in FunctionExpression and FunctionDeclaration
