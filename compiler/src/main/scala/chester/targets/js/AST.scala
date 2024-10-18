package chester.targets.js

import chester.doc.Doc
import chester.utils.doc.ToDoc

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc

// Expressions
sealed trait Expression extends ASTNode

// Unannotated Identifier
case class Identifier(
  name: String
) extends Expression {
  def toDoc: Doc = Doc.text(name)
}

// Annotated Identifier
case class TypedIdentifier(
  name: String,
  typeAnnotation: TypeAnnotation
) extends Expression {
  def toDoc: Doc = Doc.text(name) <> Doc.text(":") <+> typeAnnotation.toDoc
}

// Operators as Enums (Using Scala 2 Syntax)
sealed trait BinaryOperator
object BinaryOperator {
  case object `==` extends BinaryOperator
  case object `!=` extends BinaryOperator
  case object `===` extends BinaryOperator
  case object `!==` extends BinaryOperator
  case object `<` extends BinaryOperator
  case object `<=` extends BinaryOperator
  case object `>` extends BinaryOperator
  case object `>=` extends BinaryOperator
  case object `<<` extends BinaryOperator
  case object `>>` extends BinaryOperator
  case object `>>>` extends BinaryOperator
  case object `+` extends BinaryOperator
  case object `-` extends BinaryOperator
  case object `*` extends BinaryOperator
  case object `/` extends BinaryOperator
  case object `%` extends BinaryOperator
  case object `|` extends BinaryOperator
  case object `^` extends BinaryOperator
  case object `&` extends BinaryOperator
  case object `in` extends BinaryOperator
  case object `instanceof` extends BinaryOperator
}

sealed trait LogicalOperator
object LogicalOperator {
  case object `||` extends LogicalOperator
  case object `&&` extends LogicalOperator
  case object `??` extends LogicalOperator
}

sealed trait AssignmentOperator
object AssignmentOperator {
  case object `=` extends AssignmentOperator
  case object `+=` extends AssignmentOperator
  case object `-=` extends AssignmentOperator
  case object `*=` extends AssignmentOperator
  case object `/=` extends AssignmentOperator
  case object `%=` extends AssignmentOperator
  case object `<<=` extends AssignmentOperator
  case object `>>=` extends AssignmentOperator
  case object `>>>=` extends AssignmentOperator
  case object `|=` extends AssignmentOperator
  case object `^=` extends AssignmentOperator
  case object `&=` extends AssignmentOperator
  case object `**=` extends AssignmentOperator
}

sealed trait UnaryOperator
object UnaryOperator {
  case object `-` extends UnaryOperator
  case object `+` extends UnaryOperator
  case object `!` extends UnaryOperator
  case object `~` extends UnaryOperator
  case object `typeof` extends UnaryOperator
  case object `void` extends UnaryOperator
  case object `delete` extends UnaryOperator
}

sealed trait UpdateOperator
object UpdateOperator {
  case object `++` extends UpdateOperator
  case object `--` extends UpdateOperator
}

// Now update the AST nodes to use these enums instead of Strings

// Binary Expression
case class BinaryExpression(
  operator: BinaryOperator,
  left: Expression,
  right: Expression
) extends Expression {
  def toDoc: Doc = {
    val opDoc = Doc.text(operator.toString)
    Doc.group(left.toDoc <+> opDoc <+> right.toDoc)
  }
}

// Logical Expression
case class LogicalExpression(
  operator: LogicalOperator,
  left: Expression,
  right: Expression
) extends Expression {
  def toDoc: Doc = {
    val opDoc = Doc.text(operator.toString)
    Doc.group(left.toDoc <+> opDoc <+> right.toDoc)
  }
}

// Assignment Expression
case class AssignmentExpression(
  operator: AssignmentOperator,
  left: Expression,
  right: Expression
) extends Expression {
  def toDoc: Doc = {
    val opDoc = Doc.text(operator.toString)
    Doc.group(left.toDoc <+> opDoc <+> right.toDoc)
  }
}

// Unary Expression
case class UnaryExpression(
  operator: UnaryOperator,
  argument: Expression,
  prefix: Boolean = true
) extends Expression {
  def toDoc: Doc = {
    val opDoc = Doc.text(operator.toString)
    if (prefix)
      Doc.group(opDoc <> argument.toDoc)
    else
      Doc.group(argument.toDoc <> opDoc)
  }
}

// Update Expression
case class UpdateExpression(
  operator: UpdateOperator,
  argument: Expression,
  prefix: Boolean
) extends Expression {
  def toDoc: Doc = {
    val opDoc = Doc.text(operator.toString)
    if (prefix)
      Doc.group(opDoc <> argument.toDoc)
    else
      Doc.group(argument.toDoc <> opDoc)
  }
}

// Continue with the rest of the AST nodes, ensuring that other operator Strings are replaced by enums

// Other existing nodes remain the same...

// Conditional Expression (Ternary Operator)
case class ConditionalExpression(
  test: Expression,
  consequent: Expression,
  alternate: Expression
) extends Expression {
  def toDoc: Doc = Doc.group(test.toDoc <+> Doc.text("?") <+> consequent.toDoc <+> Doc.text(":") <+> alternate.toDoc)
}

// For brevity, I'm not including the entirety of the AST.scala file here, but you would replace all operator Strings with the corresponding enums as shown above.

// Ensure to update any pattern matches or usages of the operator fields in your code to account for the new enum types.

// Literals
sealed trait Literal extends Expression

case class NumericLiteral(value: Double) extends Literal {
  def toDoc: Doc = Doc.text(value.toString)
}

case class StringLiteral(value: String) extends Literal {
  def toDoc: Doc = Doc.text("\"" + value + "\"")
}

case class BooleanLiteral(value: Boolean) extends Literal {
  def toDoc: Doc = Doc.text(if (value) "true" else "false")
}

case object NullLiteral extends Literal {
  def toDoc: Doc = Doc.text("null")
}

case class BigIntLiteral(value: String) extends Literal {
  def toDoc: Doc = Doc.text(value + "n")
}

case class RegExpLiteral(pattern: String, flags: String) extends Literal {
  def toDoc: Doc = Doc.text(s"/$pattern/$flags")
}

// Template Literals
case class TemplateLiteral(
  quasis: List[TemplateElement],
  expressions: List[Expression]
) extends Expression {
  def toDoc: Doc = {
    val parts = quasis.zipAll(expressions, TemplateElement("", false), NullLiteral)
    val docs = parts.flatMap { case (quasi, expr) =>
      List(quasi.toDoc) ++ (expr match {
        case NullLiteral => Nil
        case e           => List(Doc.text("${") <> e.toDoc <> Doc.text("}"))
      })
    }
    Doc.text("`") <> Doc.concat(docs) <> Doc.text("`")
  }
}

case class TemplateElement(
  value: String,
  tail: Boolean
) extends ASTNode {
  def toDoc: Doc = Doc.text(value)
}

// Expressions
case class CallExpression(
  callee: Expression,
  arguments: List[Expression],
  optional: Boolean = false // For optional chaining
) extends Expression {
  def toDoc: Doc = {
    val argsDoc = Doc.sep(Doc.text(", "), arguments.map(_.toDoc))
    val optionalChar = if (optional) "?." else ""
    Doc.group(callee.toDoc <> Doc.text(optionalChar) <> Doc.text("(") <> argsDoc <> Doc.text(")"))
  }
}

case class NewExpression(
  callee: Expression,
  arguments: List[Expression]
) extends Expression {
  def toDoc: Doc = {
    val argsDoc = Doc.sep(Doc.text(", "), arguments.map(_.toDoc))
    Doc.group(Doc.text("new") <+> callee.toDoc <> Doc.text("(") <> argsDoc <> Doc.text(")"))
  }
}

case class MemberExpression(
  obj: Expression,
  property: Expression,
  computed: Boolean,
  optional: Boolean = false // For optional chaining
) extends Expression {
  def toDoc: Doc = {
    val optionalChar = if (optional) "?." else "."
    if (computed) {
      Doc.group(obj.toDoc <> Doc.text(optionalChar) <> Doc.text("[") <> property.toDoc <> Doc.text("]"))
    } else {
      Doc.group(obj.toDoc <> Doc.text(optionalChar) <> property.toDoc)
    }
  }
}

case class OptionalMemberExpression(
  obj: Expression,
  property: Expression,
  computed: Boolean,
  optional: Boolean = true
) extends Expression {
  def toDoc: Doc = MemberExpression(obj, property, computed, optional = true).toDoc
}

case class FunctionExpression(
  id: Option[Identifier],
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  async: Boolean = false,
  generator: Boolean = false
) extends Expression {
  def toDoc: Doc = {
    val asyncDoc = if (async) Doc.text("async") <+> Doc.empty else Doc.empty
    val genDoc = if (generator) Doc.text("*") else Doc.empty
    val idDoc = id.map(_.toDoc).getOrElse(Doc.empty)
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    val returnTypeDoc = returnType.map(rt => Doc.text(":") <+> rt.toDoc).getOrElse(Doc.empty)
    Doc.group(asyncDoc <> Doc.text("function") <> genDoc <+> idDoc <> paramsDoc <> returnTypeDoc <+> body.toDoc)
  }
}

case class ArrowFunctionExpression(
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: Either[Expression, BlockStatement],
  async: Boolean = false
) extends Expression {
  def toDoc: Doc = {
    val asyncDoc = if (async) Doc.text("async") <+> Doc.empty else Doc.empty
    val paramsDoc = {
      if (params.length == 1 && params.head.isSimple)
        params.head.toDoc
      else
        Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    }
    val returnTypeDoc = returnType.map(rt => Doc.text(":") <+> rt.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body match {
      case Left(expr)    => Doc.text(" => ") <> expr.toDoc
      case Right(stmts)  => Doc.text(" => ") <> stmts.toDoc
    }
    Doc.group(asyncDoc <> paramsDoc <> returnTypeDoc <> bodyDoc)
  }
}

case class ClassExpression(
  id: Option[Identifier],
  typeParameters: Option[List[TypeParameter]] = None,
  superClass: Option[Expression] = None,
  implementsInterfaces: Option[List[Expression]] = None,
  body: ClassBody,
  decorators: Option[List[Decorator]] = None
) extends Expression {
  def toDoc: Doc = {
    val decoratorsDoc = decorators.map(ds => Doc.concat(ds.map(_.toDoc))).getOrElse(Doc.empty)
    val idDoc = id.map(id => Doc.text(" ") <> id.toDoc).getOrElse(Doc.empty)
    val superClassDoc = superClass.map(sc => Doc.text(" extends ") <> sc.toDoc).getOrElse(Doc.empty)
    val implementsDoc = implementsInterfaces.map { interfaces =>
      Doc.text(" implements ") <> Doc.sep(Doc.text(", "), interfaces.map(_.toDoc))
    }.getOrElse(Doc.empty)
    decoratorsDoc <> Doc.text("class") <> idDoc <> superClassDoc <> implementsDoc <+> body.toDoc
  }
}

case class AwaitExpression(argument: Expression) extends Expression {
  def toDoc: Doc = Doc.text("await") <+> argument.toDoc
}

case class YieldExpression(
  argument: Option[Expression],
  delegate: Boolean = false
) extends Expression {
  def toDoc: Doc = {
    val delegateDoc = if (delegate) Doc.text("*") else Doc.empty
    val argDoc = argument.map(_.toDoc).getOrElse(Doc.empty)
    Doc.text("yield") <> delegateDoc <+> argDoc
  }
}

case class TaggedTemplateExpression(
  tag: Expression,
  quasi: TemplateLiteral
) extends Expression {
  def toDoc: Doc = tag.toDoc <> quasi.toDoc
}

case class SequenceExpression(expressions: List[Expression]) extends Expression {
  def toDoc: Doc = Doc.group(Doc.sep(Doc.text(", "), expressions.map(_.toDoc)))
}

case class SpreadElement(argument: Expression) extends Expression {
  def toDoc: Doc = Doc.text("...") <> argument.toDoc
}

case class ObjectExpression(
  properties: List[ObjectProperty]
) extends Expression {
  def toDoc: Doc = {
    val propsDoc = Doc.sep(Doc.text(", "), properties.map(_.toDoc))
    Doc.text("{") <> propsDoc <> Doc.text("}")
  }
}

case class ObjectProperty(
  key: Expression,
  value: Expression,
  computed: Boolean = false,
  shorthand: Boolean = false,
  method: Boolean = false
) extends ASTNode {
  def toDoc: Doc = {
    val keyDoc = if (computed) Doc.text("[") <> key.toDoc <> Doc.text("]") else key.toDoc
    if (shorthand) {
      keyDoc
    } else {
      Doc.group(keyDoc <> Doc.text(": ") <> value.toDoc)
    }
  }
}

case class ArrayExpression(
  elements: List[Option[Expression]]
) extends Expression {
  def toDoc: Doc = {
    val elemsDoc = Doc.sep(Doc.text(", "), elements.map {
      case Some(expr) => expr.toDoc
      case None       => Doc.empty
    })
    Doc.text("[") <> elemsDoc <> Doc.text("]")
  }
}

// Patterns for destructuring
sealed trait Pattern extends ASTNode

case class ObjectPattern(
  properties: List[PatternProperty],
  typeAnnotation: Option[TypeAnnotation] = None
) extends Pattern {
  def toDoc: Doc = {
    val propsDoc = Doc.sep(Doc.text(", "), properties.map(_.toDoc))
    val typeDoc = typeAnnotation.map(ta => Doc.text(": ") <> ta.toDoc).getOrElse(Doc.empty)
    Doc.text("{") <> propsDoc <> Doc.text("}") <> typeDoc
  }
}

case class ArrayPattern(
  elements: List[Option[Pattern]],
  typeAnnotation: Option[TypeAnnotation] = None
) extends Pattern {
  def toDoc: Doc = {
    val elemsDoc = Doc.sep(Doc.text(", "), elements.map {
      case Some(pat) => pat.toDoc
      case None      => Doc.empty
    })
    val typeDoc = typeAnnotation.map(ta => Doc.text(": ") <> ta.toDoc).getOrElse(Doc.empty)
    Doc.text("[") <> elemsDoc <> Doc.text("]") <> typeDoc
  }
}

case class PatternProperty(
  key: Expression,
  value: Pattern,
  computed: Boolean = false,
  shorthand: Boolean = false
) extends ASTNode {
  def toDoc: Doc = {
    val keyDoc = if (computed) Doc.text("[") <> key.toDoc <> Doc.text("]") else key.toDoc
    if (shorthand) {
      keyDoc
    } else {
      Doc.group(keyDoc <> Doc.text(": ") <> value.toDoc)
    }
  }
}

case class RestElement(
  argument: Pattern
) extends Pattern {
  def toDoc: Doc = Doc.text("...") <> argument.toDoc
}

case class AssignmentPattern(
  left: Pattern,
  right: Expression
) extends Pattern {
  def toDoc: Doc = Doc.group(left.toDoc <+> Doc.text("=") <+> right.toDoc)
}

// Statements
sealed trait Statement extends ASTNode

sealed trait Declaration extends Statement

case class VariableDeclaration(
  kind: VariableKind,
  declarations: List[VariableDeclarator]
) extends Declaration {
  def toDoc: Doc = {
    val kindDoc = kind match {
      case VariableKind.Var   => Doc.text("var")
      case VariableKind.Let   => Doc.text("let")
      case VariableKind.Const => Doc.text("const")
    }
    val declsDoc = Doc.sep(Doc.text(", "), declarations.map(_.toDoc))
    Doc.group(kindDoc <+> declsDoc <> Doc.text(";"))
  }
}

// VariableKind enumeration
sealed trait VariableKind
object VariableKind {
  case object Var extends VariableKind
  case object Let extends VariableKind
  case object Const extends VariableKind
}

case class VariableDeclarator(
  id: Pattern,
  init: Option[Expression]
) extends ASTNode {
  def toDoc: Doc = init match {
    case Some(expr) => Doc.group(id.toDoc <+> Doc.text("=") <+> expr.toDoc)
    case None       => id.toDoc
  }
}

case class FunctionDeclaration(
  id: Option[Identifier],
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  async: Boolean = false,
  generator: Boolean = false,
  typeParameters: Option[List[TypeParameter]] = None
) extends Declaration {
  def toDoc: Doc = {
    val asyncDoc = if (async) Doc.text("async") <+> Doc.empty else Doc.empty
    val genDoc = if (generator) Doc.text("*") else Doc.empty
    val idDoc = id.map(_.toDoc).getOrElse(Doc.empty)
    val typeParamsDoc = typeParameters.map { tps =>
      Doc.text("<") <> Doc.sep(Doc.text(", "), tps.map(_.toDoc)) <> Doc.text(">")
    }.getOrElse(Doc.empty)
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    val returnTypeDoc = returnType.map(rt => Doc.text(":") <+> rt.toDoc).getOrElse(Doc.empty)
    Doc.group(asyncDoc <> Doc.text("function") <> genDoc <+> idDoc <> typeParamsDoc <> paramsDoc <> returnTypeDoc <+> body.toDoc)
  }
}

case class Parameter(
  id: Pattern
) extends ASTNode {
  def toDoc: Doc = id.toDoc
  def isSimple: Boolean = id match {
    case Identifier(_)      => true
    case TypedIdentifier(_, _) => true
    case _                  => false
  }
}

case class BlockStatement(
  body: List[Statement]
) extends Statement {
  def toDoc: Doc = {
    val bodyDoc = Doc.concat(body.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("{") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.line <> Doc.text("}")
  }
}

case class ExpressionStatement(
  expression: Expression
) extends Statement {
  def toDoc: Doc = expression.toDoc <> Doc.text(";")
}

// Control Flow Statements
case class IfStatement(
  test: Expression,
  consequent: Statement,
  alternate: Option[Statement] = None
) extends Statement {
  def toDoc: Doc = {
    val ifDoc = Doc.text("if (") <> test.toDoc <> Doc.text(")") <+> consequent.toDoc
    alternate match {
      case Some(alt) => Doc.group(ifDoc <+> Doc.text("else") <+> alt.toDoc)
      case None      => ifDoc
    }
  }
}

case class ForStatement(
  init: Option[Either[VariableDeclaration, Expression]],
  test: Option[Expression],
  update: Option[Expression],
  body: Statement
) extends Statement {
  def toDoc: Doc = {
    val initDoc = init match {
      case Some(Left(varDecl))  => varDecl.toDoc
      case Some(Right(expr))    => expr.toDoc
      case None                 => Doc.empty
    }
    val testDoc = test.map(_.toDoc).getOrElse(Doc.empty)
    val updateDoc = update.map(_.toDoc).getOrElse(Doc.empty)
    Doc.group(
      Doc.text("for (") <> initDoc <> Doc.text("; ") <> testDoc <> Doc.text("; ") <> updateDoc <> Doc.text(")") <+> body.toDoc
    )
  }
}

case class WhileStatement(
  test: Expression,
  body: Statement
) extends Statement {
  def toDoc: Doc = Doc.text("while (") <> test.toDoc <> Doc.text(")") <+> body.toDoc
}

case class DoWhileStatement(
  body: Statement,
  test: Expression
) extends Statement {
  def toDoc: Doc = Doc.text("do") <+> body.toDoc <+> Doc.text("while (") <> test.toDoc <> Doc.text(");")
}

case class ForInStatement(
  left: Either[VariableDeclaration, Expression],
  right: Expression,
  body: Statement
) extends Statement {
  val leftDoc: Doc = left match {
    case Left(varDecl) => varDecl.toDoc
    case Right(expr)   => expr.toDoc
  }
  def toDoc: Doc = Doc.text("for (") <> leftDoc <> Doc.text(" in ") <> right.toDoc <> Doc.text(")") <+> body.toDoc
}

case class ForOfStatement(
  left: Either[VariableDeclaration, Expression],
  right: Expression,
  body: Statement,
  await: Boolean = false
) extends Statement {
  val leftDoc: Doc = left match {
    case Left(varDecl) => varDecl.toDoc
    case Right(expr)   => expr.toDoc
  }
  val awaitDoc = if (await) Doc.text("await") <+> Doc.empty else Doc.empty
  def toDoc: Doc = Doc.text("for") <+> awaitDoc <> Doc.text("(") <> leftDoc <> Doc.text(" of ") <> right.toDoc <> Doc.text(")") <+> body.toDoc
}

case class SwitchStatement(
  discriminant: Expression,
  cases: List[SwitchCase]
) extends Statement {
  def toDoc: Doc = {
    val casesDoc = Doc.concat(cases.map(_.toDoc))
    Doc.text("switch (") <> discriminant.toDoc <> Doc.text(")") <+> Doc.text("{") <> Doc.indent(Doc.line <> casesDoc) <> Doc.line <> Doc.text("}")
  }
}

case class SwitchCase(
  test: Option[Expression],
  consequent: List[Statement]
) extends ASTNode {
  def toDoc: Doc = {
    val testDoc = test match {
      case Some(expr) => Doc.text("case ") <> expr.toDoc <> Doc.text(":")
      case None       => Doc.text("default:")
    }
    val consDoc = Doc.concat(consequent.map(stmt => stmt.toDoc <> Doc.line))
    testDoc <> Doc.indent(Doc.line <> consDoc)
  }
}

case class BreakStatement(
  label: Option[Identifier] = None
) extends Statement {
  def toDoc: Doc = label match {
    case Some(lbl) => Doc.text("break") <+> lbl.toDoc <> Doc.text(";")
    case None      => Doc.text("break;")
  }
}

case class ContinueStatement(
  label: Option[Identifier] = None
) extends Statement {
  def toDoc: Doc = label match {
    case Some(lbl) => Doc.text("continue") <+> lbl.toDoc <> Doc.text(";")
    case None      => Doc.text("continue;")
  }
}

case class ReturnStatement(
  argument: Option[Expression]
) extends Statement {
  def toDoc: Doc = argument match {
    case Some(expr) => Doc.text("return") <+> expr.toDoc <> Doc.text(";")
    case None       => Doc.text("return;")
  }
}

case class ThrowStatement(
  argument: Expression
) extends Statement {
  def toDoc: Doc = Doc.text("throw") <+> argument.toDoc <> Doc.text(";")
}

case class TryStatement(
  block: BlockStatement,
  handler: Option[CatchClause],
  finalizer: Option[BlockStatement]
) extends Statement {
  def toDoc: Doc = {
    val tryDoc = Doc.text("try") <+> block.toDoc
    val catchDoc = handler.map(_.toDoc).getOrElse(Doc.empty)
    val finallyDoc = finalizer.map(f => Doc.text("finally") <+> f.toDoc).getOrElse(Doc.empty)
    Doc.group(tryDoc <+> catchDoc <+> finallyDoc)
  }
}

case class CatchClause(
  param: Option[Pattern],
  body: BlockStatement
) extends ASTNode {
  def toDoc: Doc = {
    val paramDoc = param.map(p => Doc.text("(") <> p.toDoc <> Doc.text(")")).getOrElse(Doc.empty)
    Doc.text("catch") <> paramDoc <+> body.toDoc
  }
}

// Classes
case class ClassDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  superClass: Option[Expression] = None,
  implementsInterfaces: Option[List[Expression]] = None,
  body: ClassBody,
  decorators: Option[List[Decorator]] = None
) extends Declaration {
  def toDoc: Doc = ClassExpression(Some(id), typeParameters, superClass, implementsInterfaces, body, decorators).toDoc
}

case class ClassBody(
  body: List[ClassElement]
) extends ASTNode {
  def toDoc: Doc = {
    val elemsDoc = Doc.concat(body.map(_.toDoc <> Doc.line))
    Doc.text("{") <> Doc.indent(Doc.line <> elemsDoc) <> Doc.line <> Doc.text("}")
  }
}

sealed trait ClassElement extends ASTNode

case class MethodDefinition(
  key: Expression,
  params: List[Parameter],
  returnType: Option[TypeAnnotation] = None,
  body: BlockStatement,
  kind: MethodKind,
  isStatic: Boolean = false,
  computed: Boolean = false,
  decorators: Option[List[Decorator]] = None
) extends ClassElement {
  def toDoc: Doc = {
    val staticDoc = if (isStatic) Doc.text("static") <+> Doc.empty else Doc.empty
    val kindDoc = kind match {
      case MethodKind.Get => Doc.text("get") <+> Doc.empty
      case MethodKind.Set => Doc.text("set") <+> Doc.empty
      case _              => Doc.empty
    }
    val keyDoc = if (computed) Doc.text("[") <> key.toDoc <> Doc.text("]") else key.toDoc
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    val returnTypeDoc = returnType.map(rt => Doc.text(":") <+> rt.toDoc).getOrElse(Doc.empty)
    val decoratorsDoc = decorators.map(ds => Doc.concat(ds.map(_.toDoc))).getOrElse(Doc.empty)
    decoratorsDoc <> staticDoc <> kindDoc <> keyDoc <> paramsDoc <> returnTypeDoc <+> body.toDoc
  }
}

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
) extends ClassElement {
  def toDoc: Doc = {
    val staticDoc = if (isStatic) Doc.text("static") <+> Doc.empty else Doc.empty
    val keyDoc = if (computed) Doc.text("[") <> key.toDoc <> Doc.text("]") else key.toDoc
    val valueDoc = value.map(v => Doc.text(" = ") <> v.toDoc).getOrElse(Doc.empty)
    val decoratorsDoc = decorators.map(ds => Doc.concat(ds.map(_.toDoc))).getOrElse(Doc.empty)
    decoratorsDoc <> staticDoc <> keyDoc <> valueDoc <> Doc.text(";")
  }
}

case class StaticBlock(
  body: List[Statement]
) extends ClassElement {
  def toDoc: Doc = {
    val bodyDoc = Doc.concat(body.map(stmt => stmt.toDoc <> Doc.line))
    Doc.text("static {") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.line <> Doc.text("}")
  }
}

// Modules: Import and Export Declarations
case class ImportDeclaration(
  specifiers: List[ImportSpecifier],
  source: StringLiteral
) extends Statement {
  def toDoc: Doc = {
    val specsDoc = Doc.sep(Doc.text(", "), specifiers.map(_.toDoc))
    Doc.text("import ") <> specsDoc <> Doc.text(" from ") <> source.toDoc <> Doc.text(";")
  }
}

sealed trait ImportSpecifier extends ASTNode

case class ImportDefaultSpecifier(
  local: Identifier
) extends ImportSpecifier {
  def toDoc: Doc = local.toDoc
}

case class ImportNamespaceSpecifier(
  local: Identifier
) extends ImportSpecifier {
  def toDoc: Doc = Doc.text("* as ") <> local.toDoc
}

case class ImportNamedSpecifier(
  local: Identifier,
  imported: Identifier
) extends ImportSpecifier {
  def toDoc: Doc = {
    if (local.name == imported.name) {
      imported.toDoc
    } else {
      imported.toDoc <> Doc.text(" as ") <> local.toDoc
    }
  }
}

sealed trait ExportDeclaration extends Statement

case class ExportNamedDeclaration(
  declaration: Option[Declaration],
  specifiers: List[ExportSpecifier],
  source: Option[StringLiteral]
) extends ExportDeclaration {
  def toDoc: Doc = {
    val declDoc = declaration.map(_.toDoc).getOrElse(Doc.empty)
    val specsDoc = if (specifiers.nonEmpty) {
      Doc.text("{") <> Doc.sep(Doc.text(", "), specifiers.map(_.toDoc)) <> Doc.text("}")
    } else {
      Doc.empty
    }
    val sourceDoc = source.map(s => Doc.text(" from ") <> s.toDoc).getOrElse(Doc.empty)
    if (declDoc != Doc.empty) {
      Doc.text("export ") <> declDoc
    } else {
      Doc.text("export ") <> specsDoc <> sourceDoc <> Doc.text(";")
    }
  }
}

case class ExportDefaultDeclaration(
  declaration: Declaration
) extends ExportDeclaration {
  def toDoc: Doc = Doc.text("export default ") <> declaration.toDoc
}

case class ExportAllDeclaration(
  source: StringLiteral,
  exported: Option[Identifier] = None
) extends ExportDeclaration {
  def toDoc: Doc = {
    val exportedDoc = exported.map(e => Doc.text(" as ") <> e.toDoc).getOrElse(Doc.empty)
    Doc.text("export * from ") <> source.toDoc <> exportedDoc <> Doc.text(";")
  }
}

case class ExportSpecifier(
  local: Identifier,
  exported: Identifier
) extends ASTNode {
  def toDoc: Doc = {
    if (local.name == exported.name) {
      local.toDoc
    } else {
      local.toDoc <> Doc.text(" as ") <> exported.toDoc
    }
  }
}

// TypeScript-specific nodes
sealed trait TypeAnnotation extends ASTNode

case class TypeReference(
  name: String,
  typeParameters: Option[List[TypeAnnotation]] = None
) extends TypeAnnotation {
  def toDoc: Doc = {
    val paramsDoc = typeParameters.map { tps =>
      Doc.text("<") <> Doc.sep(Doc.text(", "), tps.map(_.toDoc)) <> Doc.text(">")
    }.getOrElse(Doc.empty)
    Doc.text(name) <> paramsDoc
  }
}

case class FunctionTypeAnnotation(
  params: List[Parameter],
  returnType: TypeAnnotation
) extends TypeAnnotation {
  def toDoc: Doc = {
    val paramsDoc = Doc.text("(") <> Doc.sep(Doc.text(", "), params.map(_.toDoc)) <> Doc.text(")")
    paramsDoc <> Doc.text(" => ") <> returnType.toDoc
  }
}

// Union and Intersection Types
case class UnionType(
  types: List[TypeAnnotation]
) extends TypeAnnotation {
  def toDoc: Doc = Doc.sep(Doc.text(" | "), types.map(_.toDoc))
}

case class IntersectionType(
  types: List[TypeAnnotation]
) extends TypeAnnotation {
  def toDoc: Doc = Doc.sep(Doc.text(" & "), types.map(_.toDoc))
}

// Generics
case class GenericTypeAnnotation(
  id: Identifier,
  typeParameters: Option[List[TypeAnnotation]] = None
) extends TypeAnnotation {
  def toDoc: Doc = TypeReference(id.name, typeParameters).toDoc
}

// Primitive Type Annotations
case object AnyTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("any")
}

case object UnknownTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("unknown")
}

case object NumberTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("number")
}

case object StringTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("string")
}

case object BooleanTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("boolean")
}

case object NullTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("null")
}

case object UndefinedTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("undefined")
}

case object NeverTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("never")
}

case object ObjectTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("object")
}

case object SymbolTypeAnnotation extends TypeAnnotation {
  def toDoc: Doc = Doc.text("symbol")
}

// Type Parameters
case class TypeParameter(
  name: String,
  constraint: Option[TypeAnnotation] = None,
  default: Option[TypeAnnotation] = None
) extends ASTNode {
  def toDoc: Doc = {
    val constraintDoc = constraint.map(c => Doc.text(" extends ") <> c.toDoc).getOrElse(Doc.empty)
    val defaultDoc = default.map(d => Doc.text(" = ") <> d.toDoc).getOrElse(Doc.empty)
    Doc.text(name) <> constraintDoc <> defaultDoc
  }
}

// Enums and Type Aliases (TypeScript)
case class EnumDeclaration(
  id: Identifier,
  members: List[EnumMember]
) extends Declaration {
  def toDoc: Doc = {
    val membersDoc = Doc.concat(members.map(m => m.toDoc <> Doc.text(",") <> Doc.line))
    Doc.text("enum ") <> id.toDoc <+> Doc.text("{") <> Doc.indent(Doc.line <> membersDoc) <> Doc.line <> Doc.text("}")
  }
}

case class EnumMember(
  id: Identifier,
  initializer: Option[Expression] = None
) extends ASTNode {
  def toDoc: Doc = initializer match {
    case Some(init) => id.toDoc <> Doc.text(" = ") <> init.toDoc
    case None       => id.toDoc
  }
}

case class TypeAliasDeclaration(
  id: Identifier,
  typeParameters: Option[List[TypeParameter]] = None,
  typeAnnotation: TypeAnnotation
) extends Declaration {
  def toDoc: Doc = {
    val typeParamsDoc = typeParameters.map { tps =>
      Doc.text("<") <> Doc.sep(Doc.text(", "), tps.map(_.toDoc)) <> Doc.text(">")
    }.getOrElse(Doc.empty)
    Doc.text("type ") <> id.toDoc <> typeParamsDoc <> Doc.text(" = ") <> typeAnnotation.toDoc <> Doc.text(";")
  }
}

// Type Assertions and As Expressions (TypeScript)
case class TypeAssertion(
  typeAnnotation: TypeAnnotation,
  expression: Expression
) extends Expression {
  def toDoc: Doc = Doc.text("<") <> typeAnnotation.toDoc <> Doc.text(">") <> expression.toDoc
}

case class AsExpression(
  expression: Expression,
  typeAnnotation: TypeAnnotation
) extends Expression {
  def toDoc: Doc = expression.toDoc <+> Doc.text("as ") <> typeAnnotation.toDoc
}

case class NonNullExpression(
  expression: Expression
) extends Expression {
  def toDoc: Doc = expression.toDoc <> Doc.text("!")
}

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
