package chester.target.js

// Base trait for all AST nodes
sealed trait ASTNode

// Expressions
sealed trait Expression extends ASTNode

case class Identifier(name: String) extends Expression

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

case class VariableDeclaration(
  kind: String, // "var", "let", or "const"
  declarations: List[VariableDeclarator]
) extends Statement

case class VariableDeclarator(
  id: Identifier, 
  init: Option[Expression]
) extends ASTNode

case class FunctionDeclaration(
  id: Identifier, 
  params: List[Parameter], 
  returnType: Option[TypeAnnotation],
  body: BlockStatement
) extends Statement

case class Parameter(
  id: Identifier, 
  typeAnnotation: Option[TypeAnnotation]
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

// Extend existing nodes with optional type annotations
case class TypedIdentifier(
  name: String, 
  typeAnnotation: Option[TypeAnnotation]
) extends Expression

// Update VariableDeclarator to hold TypedIdentifier
case class TypedVariableDeclarator(
  id: TypedIdentifier, 
  init: Option[Expression]
) extends ASTNode

// Update FunctionDeclaration to hold TypedIdentifiers and returnType
case class TypedFunctionDeclaration(
  id: TypedIdentifier, 
  params: List[Parameter], 
  returnType: Option[TypeAnnotation],
  body: BlockStatement
) extends Statement

// Interfaces and Classes (TypeScript)
case class InterfaceDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]],
  extendsInterfaces: Option[List[Expression]],
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
  typeParameters: Option[List[TypeParameter]],
  superClass: Option[Expression],
  body: ClassBody
) extends Statement

case class ClassBody(
  body: List[ClassElement]
) extends ASTNode

sealed trait ClassElement extends ASTNode

case class MethodDefinition(
  key: Identifier,
  params: List[Parameter],
  returnType: Option[TypeAnnotation],
  body: BlockStatement,
  isStatic: Boolean,
  kind: String // "constructor", "method", "get", "set"
) extends ClassElement

case class PropertyDefinition(
  key: Identifier,
  typeAnnotation: Option[TypeAnnotation],
  initializer: Option[Expression],
  isStatic: Boolean
) extends ClassElement

// Type Parameters
case class TypeParameter(
  name: String,
  constraint: Option[TypeAnnotation]
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
  typeParameters: Option[List[TypeAnnotation]]
) extends TypeAnnotation
