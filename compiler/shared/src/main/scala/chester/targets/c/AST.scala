package chester.targets.c

import chester.utils.doc._
import upickle.default._

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc derives ReadWriter

// Expressions
sealed trait Expression extends ASTNode derives ReadWriter

// Identifiers
case class Identifier(
    name: String
) extends Expression {
  def toDoc(using  PrettierOptions): Doc = Doc.text(name)
}

// Literals
sealed trait Literal extends Expression derives ReadWriter

case class IntegerLiteral(value: Long) extends Literal {
  def toDoc(using  PrettierOptions): Doc = Doc.text(value.toString)
}

case class FloatLiteral(value: Double) extends Literal {
  def toDoc(using  PrettierOptions): Doc = Doc.text(value.toString)
}

case class CharLiteral(value: Char) extends Literal {
  def toDoc(using  PrettierOptions): Doc = Doc.text(s"'$value'")
}

case class StringLiteral(value: String) extends Literal {
  def toDoc(using  PrettierOptions): Doc = Doc.text(s""""$value"""")
}

// Binary Expressions
case class BinaryExpression(
    operator: BinaryOperator,
    left: Expression,
    right: Expression
) extends Expression {
  def toDoc(using  PrettierOptions): Doc =
    Doc.group(left.toDoc <+> Doc.text(operator.toString) <+> right.toDoc)
}

// Unary Expressions
case class UnaryExpression(
    operator: UnaryOperator,
    argument: Expression
) extends Expression {
  def toDoc(using  PrettierOptions): Doc =
    Doc.group(Doc.text(operator.toString) <> argument.toDoc)
}

// Function Call
case class CallExpression(
    callee: Expression,
    arguments: List[Expression]
) extends Expression {
  def toDoc(using  PrettierOptions): Doc = {
    val argsDoc = Doc.sep(Doc.text(", "), arguments.map(_.toDoc))
    Doc.group(callee.toDoc <> Doc.text("(") <> argsDoc <> Doc.text(")"))
  }
}

// Initializer for variable declarations
case class Initializer(
    expressions: List[Expression]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = {
    val exprsDoc = Doc.sep(Doc.text(", "), expressions.map(_.toDoc))
    Doc.text("{") <> exprsDoc <> Doc.text("}")
  }
}

// Statements
sealed trait Statement extends ASTNode derives ReadWriter

case class ExpressionStatement(
    expression: Expression
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = expression.toDoc <> Doc.text(";")
}

case class ReturnStatement(
    argument: Option[Expression]
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = argument match {
    case Some(expr) => Doc.text("return") <+> expr.toDoc <> Doc.text(";")
    case None       => Doc.text("return;")
  }
}

// Declarations
sealed trait Declaration extends Statement derives ReadWriter

case class VariableDeclaration(
    typeSpecifier: TypeSpecifier,
    declarators: List[VariableDeclarator]
) extends Declaration {
  def toDoc(using  PrettierOptions): Doc = {
    val declsDoc = Doc.sep(Doc.text(", "), declarators.map(_.toDoc))
    Doc.group(typeSpecifier.toDoc <+> declsDoc <> Doc.text(";"))
  }
}

case class VariableDeclarator(
    id: Identifier,
    pointer: Option[PointerType],
    init: Option[Either[Expression, Initializer]]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = {
    val pointerDoc = pointer.map(_.toDoc).getOrElse(Doc.empty)
    val initDoc = init
      .map {
        case Left(expr)    => Doc.text(" = ") <> expr.toDoc
        case Right(initzr) => Doc.text(" = ") <> initzr.toDoc
      }
      .getOrElse(Doc.empty)
    Doc.group(pointerDoc <> id.toDoc <> initDoc)
  }
}

case class FunctionDeclaration(
    returnType: TypeSpecifier,
    id: Identifier,
    params: List[Parameter],
    body: BlockStatement
) extends Declaration {
  def toDoc(using  PrettierOptions): Doc = {
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    Doc.group(
      returnType.toDoc <+> id.toDoc <> paramsDoc <+> body.toDoc
    )
  }
}

case class Parameter(
    typeSpecifier: TypeSpecifier,
    id: Identifier
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = typeSpecifier.toDoc <+> id.toDoc
}

case class BlockStatement(
    body: List[Statement]
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = {
    val bodyDoc = Doc.concat(body.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("{") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.line <> Doc.text("}")
  }
}

// Type Specifiers
sealed trait TypeSpecifier extends ASTNode derives ReadWriter

// Basic Types
case object IntType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("int")
}

case object UnsignedIntType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("unsigned int")
}

case object ShortIntType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("short int")
}

case object LongIntType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("long int")
}

case object FloatType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("float")
}

case object DoubleType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("double")
}

case object CharType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("char")
}

case object UnsignedCharType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("unsigned char")
}

case object VoidType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("void")
}

case object BoolType extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text("bool")
}

// Pointer Type
case class PointerType(
    baseType: TypeSpecifier
) extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = baseType.toDoc <> Doc.text("*")
}

// Array Type
case class ArrayType(
    baseType: TypeSpecifier,
    size: Option[Expression]
) extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = {
    val sizeDoc = size.map(_.toDoc).getOrElse(Doc.empty)
    baseType.toDoc <> Doc.text("[") <> sizeDoc <> Doc.text("]")
  }
}

// Custom Type (e.g., typedefs, structs)
case class CustomType(
    name: String
) extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = Doc.text(name)
}

// Struct Type
case class StructType(
    id: Option[Identifier],
    members: Option[List[StructMember]]
) extends TypeSpecifier {
  def toDoc(using  PrettierOptions): Doc = {
    val idDoc = id.map(_.toDoc).getOrElse(Doc.empty)
    val membersDoc = members
      .map { m =>
        val bodyDoc = Doc.concat(m.map(member => member.toDoc <> Doc.text(";") <> Doc.line))
        Doc.text("{") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.line <> Doc.text("}")
      }
      .getOrElse(Doc.empty)
    Doc.text("struct") <+> idDoc <> membersDoc
  }
}

// Struct Declaration
case class StructDeclaration(
    id: Identifier,
    members: List[StructMember]
) extends Declaration {
  def toDoc(using  PrettierOptions): Doc = {
    val membersDoc = Doc.concat(members.map(m => m.toDoc <> Doc.text(";") <> Doc.line))
    Doc.text("struct") <+> id.toDoc <+> Doc.text("{") <>
      Doc.indent(Doc.line <> membersDoc) <> Doc.line <> Doc.text("};")
  }
}

case class StructMember(
    typeSpecifier: TypeSpecifier,
    id: Identifier
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = typeSpecifier.toDoc <+> id.toDoc
}

// Typedef Declaration
case class TypedefDeclaration(
    originalType: TypeSpecifier,
    newType: CustomType
) extends Declaration {
  def toDoc(using  PrettierOptions): Doc =
    Doc.text("typedef") <+> originalType.toDoc <+> newType.toDoc <> Doc.text(";")
}

// Enum Declaration
case class EnumDeclaration(
    id: Identifier,
    members: List[EnumMember]
) extends Declaration {
  def toDoc(using  PrettierOptions): Doc = {
    val membersDoc = Doc.sep(Doc.text(", "), members.map(_.toDoc))
    Doc.text("enum") <+> id.toDoc <+> Doc.text("{") <>
      Doc.indent(Doc.line <> membersDoc) <> Doc.line <> Doc.text("};")
  }
}

case class EnumMember(
    id: Identifier,
    value: Option[Expression]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = value match {
    case Some(expr) => id.toDoc <+> Doc.text("=") <+> expr.toDoc
    case None       => id.toDoc
  }
}

// Operators
enum BinaryOperator derives ReadWriter {
  case Add, Subtract, Multiply, Divide,
    Modulo, And, Or, Equal, NotEqual,
    LessThan, GreaterThan, LessThanOrEqual, GreaterThanOrEqual,
    BitwiseAnd, BitwiseOr, BitwiseXor, LeftShift, RightShift,
    Assign, AddAssign, SubtractAssign, MultiplyAssign, DivideAssign,
    ModuloAssign, AndAssign, OrAssign, XorAssign, LeftShiftAssign, RightShiftAssign

  override def toString: String = this match {
    case Add                => "+"
    case Subtract           => "-"
    case Multiply           => "*"
    case Divide             => "/"
    case Modulo             => "%"
    case And                => "&&"
    case Or                 => "||"
    case Equal              => "=="
    case NotEqual           => "!="
    case LessThan           => "<"
    case GreaterThan        => ">"
    case LessThanOrEqual    => "<="
    case GreaterThanOrEqual => ">="
    case BitwiseAnd         => "&"
    case BitwiseOr          => "|"
    case BitwiseXor         => "^"
    case LeftShift          => "<<"
    case RightShift         => ">>"
    case Assign             => "="
    case AddAssign          => "+="
    case SubtractAssign     => "-="
    case MultiplyAssign     => "*="
    case DivideAssign       => "/="
    case ModuloAssign       => "%="
    case AndAssign          => "&="
    case OrAssign           => "|="
    case XorAssign          => "^="
    case LeftShiftAssign    => "<<="
    case RightShiftAssign   => ">>="
  }
}

enum UnaryOperator derives ReadWriter {
  case Negate, Not, AddressOf, Dereference, BitwiseNot, Increment, Decrement

  override def toString: String = this match {
    case Negate      => "-"
    case Not         => "!"
    case AddressOf   => "&"
    case Dereference => "*"
    case BitwiseNot  => "~"
    case Increment   => "++"
    case Decrement   => "--"
  }
}

// Control Flow Statements
case class IfStatement(
    test: Expression,
    consequent: Statement,
    alternate: Option[Statement]
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = {
    val ifDoc = Doc.text("if (") <> test.toDoc <> Doc.text(")") <+> consequent.toDoc
    alternate match {
      case Some(alt) => Doc.group(ifDoc <+> Doc.text("else") <+> alt.toDoc)
      case None      => ifDoc
    }
  }
}

case class WhileStatement(
    test: Expression,
    body: Statement
) extends Statement {
  def toDoc(using  PrettierOptions): Doc =
    Doc.text("while (") <> test.toDoc <> Doc.text(")") <+> body.toDoc
}

case class DoWhileStatement(
    body: Statement,
    test: Expression
) extends Statement {
  def toDoc(using  PrettierOptions): Doc =
    Doc.text("do") <+> body.toDoc <+> Doc.text("while (") <> test.toDoc <> Doc.text(");")
}

case class ForStatement(
    init: Option[Statement],
    test: Option[Expression],
    update: Option[Expression],
    body: Statement
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = {
    val initDoc = init.map(_.toDoc).getOrElse(Doc.empty)
    val testDoc = test.map(_.toDoc).getOrElse(Doc.empty)
    val updateDoc = update.map(_.toDoc).getOrElse(Doc.empty)
    Doc.group(
      Doc.text("for (") <> initDoc <> Doc.text("; ") <> testDoc <> Doc.text("; ") <> updateDoc <> Doc.text(")") <+> body.toDoc
    )
  }
}

case class SwitchStatement(
    discriminant: Expression,
    cases: List[CaseStatement],
    defaultCase: Option[DefaultCase]
) extends Statement {
  def toDoc(using  PrettierOptions): Doc = {
    val casesDoc = Doc.concat(cases.map(_.toDoc <> Doc.line))
    val defaultDoc = defaultCase.map(_.toDoc <> Doc.line).getOrElse(Doc.empty)
    Doc.group(
      Doc.text("switch (") <> discriminant.toDoc <> Doc.text(")") <+> Doc.text("{") <>
        Doc.indent(Doc.line <> casesDoc <> defaultDoc) <> Doc.line <> Doc.text("}")
    )
  }
}

case class CaseStatement(
    test: Expression,
    consequent: List[Statement]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = {
    val bodyDoc = Doc.concat(consequent.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("case ") <> test.toDoc <> Doc.text(":") <> Doc.line <> Doc.indent(bodyDoc)
  }
}

case class DefaultCase(
    consequent: List[Statement]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = {
    val bodyDoc = Doc.concat(consequent.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("default:") <> Doc.line <> Doc.indent(bodyDoc)
  }
}

case class GotoStatement(
    label: Identifier
) extends Statement {
  def toDoc(using  PrettierOptions): Doc =
    Doc.text("goto") <+> label.toDoc <> Doc.text(";")
}

case class LabelStatement(
    label: Identifier,
    statement: Statement
) extends Statement {
  def toDoc(using  PrettierOptions): Doc =
    label.toDoc <> Doc.text(":") <> Doc.line <> statement.toDoc
}

// Preprocessor Directives
sealed trait PreprocessorDirective extends ASTNode derives ReadWriter

case class IncludeDirective(
    path: String,
    isSystem: Boolean
) extends PreprocessorDirective {
  def toDoc(using  PrettierOptions): Doc =
    if (isSystem) Doc.text(s"#include <$path>")
    else Doc.text(s"""#include "$path"""")
}

case class DefineDirective(
    name: String,
    value: Option[String]
) extends PreprocessorDirective {
  def toDoc(using  PrettierOptions): Doc = value match {
    case Some(v) => Doc.text(s"#define $name $v")
    case None    => Doc.text(s"#define $name")
  }
}

case class IfDefDirective(
    condition: String,
    trueBranch: List[ASTNode],
    falseBranch: Option[List[ASTNode]]
) extends PreprocessorDirective {
  def toDoc(using  PrettierOptions): Doc = {
    val trueDoc = Doc.concat(trueBranch.map(_.toDoc <> Doc.line))
    val falseDoc = falseBranch
      .map { fb =>
        Doc.text("#else") <> Doc.line <> Doc.concat(fb.map(_.toDoc <> Doc.line))
      }
      .getOrElse(Doc.empty)
    Doc.text(s"#ifdef $condition") <> Doc.line <> trueDoc <> falseDoc <> Doc.text("#endif")
  }
}

// Comments
case class Comment(
    content: String,
    isBlock: Boolean
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc =
    if (isBlock) Doc.text(s"/* $content */")
    else Doc.text(s"// $content")
}

// Top-level Program
case class Program(
    declarations: List[Declaration],
    preprocessorDirectives: List[PreprocessorDirective]
) extends ASTNode {
  def toDoc(using  PrettierOptions): Doc = {
    val directivesDoc = Doc.concat(preprocessorDirectives.map(_.toDoc <> Doc.line))
    val declarationsDoc = Doc.concat(declarations.map(_.toDoc <> Doc.line))
    directivesDoc <> Doc.line <> declarationsDoc
  }
}
