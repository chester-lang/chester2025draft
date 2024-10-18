package chester.targets.c

import chester.utils.doc.*
import upickle.default.*

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc derives ReadWriter

// Expressions
sealed trait Expression extends ASTNode derives ReadWriter

// Identifiers
case class Identifier(
    name: String
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(name)
}

// Literals
sealed trait Literal extends Expression derives ReadWriter

case class IntegerLiteral(value: Long) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(value.toString)
}

case class FloatLiteral(value: Double) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(value.toString)
}

case class CharLiteral(value: Char) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(s"'$value'")
}

case class StringLiteral(value: String) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(s""""$value"""")
}

// Binary Expressions
case class BinaryExpression(
    operator: BinaryOperator,
    left: Expression,
    right: Expression
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc =
    Doc.group(left.toDoc <+> Doc.text(operator.toString) <+> right.toDoc)
}

// Unary Expressions
case class UnaryExpression(
    operator: UnaryOperator,
    argument: Expression
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc =
    Doc.group(Doc.text(operator.toString) <> argument.toDoc)
}

// Function Call
case class CallExpression(
    callee: Expression,
    arguments: List[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = Doc.sep(Doc.text(","), arguments.map(_.toDoc))
    Doc.group(callee.toDoc <> Doc.text("(") <> argsDoc <> Doc.text(")"))
  }
}

// Statements
sealed trait Statement extends ASTNode derives ReadWriter

case class ExpressionStatement(
    expression: Expression
) extends Statement {
  def toDoc(using options: PrettierOptions): Doc = expression.toDoc <> Doc.text(";")
}

case class ReturnStatement(
    argument: Option[Expression]
) extends Statement {
  def toDoc(using options: PrettierOptions): Doc = argument match {
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
  def toDoc(using options: PrettierOptions): Doc = {
    val declsDoc = Doc.sep(Doc.text(","), declarators.map(_.toDoc))
    Doc.group(typeSpecifier.toDoc <+> declsDoc <> Doc.text(";"))
  }
}

case class VariableDeclarator(
    id: Identifier,
    init: Option[Expression]
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = init match {
    case Some(expr) => Doc.group(id.toDoc <+> Doc.text("=") <+> expr.toDoc)
    case None       => id.toDoc
  }
}

case class FunctionDeclaration(
    returnType: TypeSpecifier,
    id: Identifier,
    params: List[Parameter],
    body: BlockStatement
) extends Declaration {
  def toDoc(using options: PrettierOptions): Doc = {
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(","), params.map(_.toDoc)) <> Doc.text(")")
    Doc.group(
      returnType.toDoc <+> id.toDoc <> paramsDoc <+> body.toDoc
    )
  }
}

case class Parameter(
    typeSpecifier: TypeSpecifier,
    id: Identifier
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = typeSpecifier.toDoc <+> id.toDoc
}

case class BlockStatement(
    body: List[Statement]
) extends Statement {
  def toDoc(using options: PrettierOptions): Doc = {
    val bodyDoc = Doc.concat(body.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("{") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.line <> Doc.text("}")
  }
}

// Type Specifiers
sealed trait TypeSpecifier extends ASTNode derives ReadWriter

case object IntType extends TypeSpecifier {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("int")
}

case object FloatType extends TypeSpecifier {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("float")
}

case object DoubleType extends TypeSpecifier {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("double")
}

case object CharType extends TypeSpecifier {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("char")
}

case object VoidType extends TypeSpecifier {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("void")
}

// Operators
sealed trait BinaryOperator derives ReadWriter
object BinaryOperator {
  case object Add extends BinaryOperator { override def toString = "+" }
  case object Subtract extends BinaryOperator { override def toString = "-" }
  case object Multiply extends BinaryOperator { override def toString = "*" }
  case object Divide extends BinaryOperator { override def toString = "/" }
  // Add more binary operators as needed
}

sealed trait UnaryOperator derives ReadWriter
object UnaryOperator {
  case object Negate extends UnaryOperator { override def toString = "-" }
  case object Not extends UnaryOperator { override def toString = "!" }
  // Add more unary operators as needed
}

// Control Flow Statements
case class IfStatement(
    test: Expression,
    consequent: Statement,
    alternate: Option[Statement]
) extends Statement {
  def toDoc(using options: PrettierOptions): Doc = {
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
  def toDoc(using options: PrettierOptions): Doc = 
    Doc.text("while (") <> test.toDoc <> Doc.text(")") <+> body.toDoc
}

case class ForStatement(
    init: Option[Statement],
    test: Option[Expression],
    update: Option[Expression],
    body: Statement
) extends Statement {
  def toDoc(using options: PrettierOptions): Doc = {
    val initDoc = init.map(_.toDoc).getOrElse(Doc.empty)
    val testDoc = test.map(_.toDoc).getOrElse(Doc.empty)
    val updateDoc = update.map(_.toDoc).getOrElse(Doc.empty)
    Doc.group(
      Doc.text("for (") <> initDoc <> Doc.text("; ") <> testDoc <> Doc.text("; ") <> updateDoc <> Doc.text(")") <+> body.toDoc
    )
  }
}

// Struct Declaration
case class StructDeclaration(
    id: Identifier,
    members: List[StructMember]
) extends Declaration {
  def toDoc(using options: PrettierOptions): Doc = {
    val membersDoc = Doc.concat(members.map(m => m.toDoc <> Doc.text(";") <> Doc.line))
    Doc.text("struct") <+> id.toDoc <+> Doc.text("{") <> Doc.indent(Doc.line <> membersDoc) <> Doc.line <> Doc.text("};")
  }
}

case class StructMember(
    typeSpecifier: TypeSpecifier,
    id: Identifier
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = typeSpecifier.toDoc <+> id.toDoc
}

// Typedef
case class TypedefDeclaration(
    typeSpecifier: TypeSpecifier,
    id: Identifier
) extends Declaration {
  def toDoc(using options: PrettierOptions): Doc =
    Doc.text("typedef") <+> typeSpecifier.toDoc <+> id.toDoc <> Doc.text(";")
}

// Enum Declaration
case class EnumDeclaration(
    id: Identifier,
    members: List[EnumMember]
) extends Declaration {
  def toDoc(using options: PrettierOptions): Doc = {
    val membersDoc = Doc.sep(Doc.text(","), members.map(_.toDoc))
    Doc.text("enum") <+> id.toDoc <+> Doc.text("{") <> Doc.indent(Doc.line <> membersDoc) <> Doc.line <> Doc.text("};")
  }
}

case class EnumMember(
    id: Identifier,
    value: Option[Expression]
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = value match {
    case Some(expr) => id.toDoc <+> Doc.text("=") <+> expr.toDoc
    case None       => id.toDoc
  }
}

// Preprocessor Directives
sealed trait PreprocessorDirective extends ASTNode derives ReadWriter

case class IncludeDirective(path: String, isSystem: Boolean) extends PreprocessorDirective {
  def toDoc(using options: PrettierOptions): Doc = 
    if (isSystem) Doc.text(s"#include <$path>")
    else Doc.text(s"""#include "$path"""")
}

case class DefineDirective(name: String, value: Option[String]) extends PreprocessorDirective {
  def toDoc(using options: PrettierOptions): Doc = value match {
    case Some(v) => Doc.text(s"#define $name $v")
    case None    => Doc.text(s"#define $name")
  }
}

// Top-level Program
case class Program(
    declarations: List[Declaration],
    preprocessorDirectives: List[PreprocessorDirective]
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = {
    val directivesDoc = Doc.concat(preprocessorDirectives.map(_.toDoc <> Doc.line))
    val declarationsDoc = Doc.concat(declarations.map(_.toDoc <> Doc.line))
    directivesDoc <> Doc.line <> declarationsDoc
  }
}
