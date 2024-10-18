package chester.targets.js

// Base trait for all AST nodes
sealed trait ASTNode

// Expressions
sealed trait Expression extends ASTNode

case class Identifier(
  name: String, 
  typeAnnotation: Option[TypeAnnotation] = None
) extends Expression

case class Literal(value: Any) extends Expression

case class BinaryExpression(
  operator: String, 
  left: Expression, 
  right: Expression
) extends Expression

case class CallExpression(
  callee: Expression, 
  arguments: List[Expression]
) extends Expression

case class MemberExpression(
  obj: Expression, 
  property: Expression, 
  computed: Boolean
) extends Expression

// Statements
sealed trait Statement extends ASTNode

// VariableKind enumeration
sealed trait VariableKind
object VariableKind {
  case object Var extends VariableKind
  case object Let extends VariableKind
  case object Const extends VariableKind
}

case class VariableDeclaration(
  kind: VariableKind,
  declarations: List[VariableDeclarator]
) extends Statement

case class VariableDeclarator(
  id: Identifier, 
  init: Option[Expression]
) extends ASTNode

case class FunctionDeclaration(
  id: Identifier, 
  params: List[Parameter], 
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement
) extends Statement

case class Parameter(
  id: Identifier
) extends ASTNode

case class BlockStatement(
  body: List[Statement]
) extends Statement

case class ExpressionStatement(
  expression: Expression
) extends Statement

case class ReturnStatement(
  argument: Option[Expression]
) extends Statement

// TypeScript-specific nodes
sealed trait TypeAnnotation extends ASTNode

case class TypeReference(
  name: String
) extends TypeAnnotation

case class FunctionTypeAnnotation(
  params: List[Parameter],
  returnType: TypeAnnotation
) extends TypeAnnotation

// Interfaces and Classes (TypeScript)
case class InterfaceDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  extendsInterfaces: Option[List[Expression]] = None,
  body: InterfaceBody
) extends Statement

case class InterfaceBody(
  properties: List[InterfaceProperty]
) extends ASTNode

case class InterfaceProperty(
  key: Identifier,
  value: TypeAnnotation,
  optional: Boolean
) extends ASTNode

case class ClassDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  superClass: Option[Expression] = None,
  body: ClassBody
) extends Statement

case class ClassBody(
  body: List[ClassElement]
) extends ASTNode

sealed trait ClassElement extends ASTNode

case class MethodDefinition(
  key: Identifier,
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  isStatic: Boolean,
  kind: String // "constructor", "method", "get", "set"
) extends ClassElement

case class PropertyDefinition(
  key: Identifier,
  initializer: Option[Expression] = None,
  isStatic: Boolean
) extends ClassElement

// Type Parameters
case class TypeParameter(
  name: String,
  constraint: Option[TypeAnnotation] = None
) extends ASTNode

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
