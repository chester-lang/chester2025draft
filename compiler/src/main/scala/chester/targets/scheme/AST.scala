package chester.targets.scheme

import chester.utils.doc.*
import upickle.default.*

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc derives ReadWriter

// Expressions
sealed trait Expression extends ASTNode derives ReadWriter

// Scheme literals
sealed trait Literal extends Expression derives ReadWriter

case class StringLiteral(value: String) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(s""""$value"""")
}

case class NumberLiteral(value: Double) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(value.toString)
}

case class BooleanLiteral(value: Boolean) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = if (value) Doc.text("#t") else Doc.text("#f")
}

case object NullLiteral extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("'()")
}

// Identifiers
case class Identifier(name: String) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(name)
}

// List (S-expression)
case class ListExpression(elements: List[Expression]) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val elemsDoc = Doc.sep(Doc.text(" "), elements.map(_.toDoc))
    Doc.text("(") <> elemsDoc <> Doc.text(")")
  }
}

// Define expression
case class DefineExpression(
    name: Identifier,
    value: Expression
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc =
    Doc.text("(define ") <> name.toDoc <+> value.toDoc <> Doc.text(")")
}

// Lambda expression
case class LambdaExpression(
    parameters: List[Identifier],
    body: List[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val paramsDoc = Doc.sep(Doc.text(" "), parameters.map(_.toDoc))
    val bodyDoc = Doc.concat(body.map(_.toDoc <> Doc.line))
    Doc.text("(lambda (") <> paramsDoc <> Doc.text(")") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.text(")")
  }
}

// If expression
case class IfExpression(
    condition: Expression,
    thenBranch: Expression,
    elseBranch: Option[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val elseDoc = elseBranch.map(e => Doc.text(" ") <> e.toDoc).getOrElse(Doc.empty)
    Doc.text("(if ") <> condition.toDoc <+> thenBranch.toDoc <> elseDoc <> Doc.text(")")
  }
}

// Set! expression
case class SetExpression(
    name: Identifier,
    value: Expression
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc =
    Doc.text("(set! ") <> name.toDoc <+> value.toDoc <> Doc.text(")")
}

// Begin expression
case class BeginExpression(
    expressions: List[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val exprsDoc = Doc.concat(expressions.map(_.toDoc <> Doc.line))
    Doc.text("(begin") <> Doc.indent(Doc.line <> exprsDoc) <> Doc.line <> Doc.text(")")
  }
}

// Let expression
case class LetExpression(
    bindings: List[(Identifier, Expression)],
    body: List[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val bindingsDoc = Doc.concat(bindings.map { case (id, expr) =>
      Doc.text("(") <> id.toDoc <+> expr.toDoc <> Doc.text(")")
    })
    val bodyDoc = Doc.concat(body.map(_.toDoc <> Doc.line))
    Doc.text("(let (") <> bindingsDoc <> Doc.text(")") <> Doc.indent(Doc.line <> bodyDoc) <> Doc.text(")")
  }
}

// Cond expression
case class CondExpression(
    clauses: List[(Expression, List[Expression])]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val clausesDoc = Doc.concat(clauses.map { case (test, exprs) =>
      val exprsDoc = Doc.concat(exprs.map(_.toDoc <> Doc.line))
      Doc.text("(") <> test.toDoc <> Doc.indent(Doc.line <> exprsDoc) <> Doc.text(")")
    })
    Doc.text("(cond") <> Doc.indent(Doc.line <> clausesDoc) <> Doc.line <> Doc.text(")")
  }
}

// Case expression
case class CaseExpression(
    key: Expression,
    clauses: List[(List[Literal], List[Expression])],
    elseClause: Option[List[Expression]] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val clausesDoc = Doc.concat(clauses.map { case (datums, exprs) =>
      val datumsDoc = Doc.sep(Doc.text(" "), datums.map(_.toDoc))
      val exprsDoc = Doc.concat(exprs.map(_.toDoc <> Doc.line))
      Doc.text("(") <> Doc.text("(") <> datumsDoc <> Doc.text(")") <> Doc.indent(Doc.line <> exprsDoc) <> Doc.text(")")
    })
    val elseDoc = elseClause
      .map { exprs =>
        val exprsDoc = Doc.concat(exprs.map(_.toDoc <> Doc.line))
        Doc.text("(else") <> Doc.indent(Doc.line <> exprsDoc) <> Doc.text(")")
      }
      .getOrElse(Doc.empty)
    Doc.text("(case ") <> key.toDoc <> Doc.indent(Doc.line <> clausesDoc <> elseDoc) <> Doc.line <> Doc.text(")")
  }
}

// Do expression
case class DoExpression(
    variables: List[(Identifier, Expression, Option[Expression])],
    test: Expression,
    commands: List[Expression],
    body: List[Expression]
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = {
    val varsDoc = Doc.concat(variables.map { case (id, init, stepOpt) =>
      val stepDoc = stepOpt.map(step => Doc.text(" ") <> step.toDoc).getOrElse(Doc.empty)
      Doc.text("(") <> id.toDoc <+> init.toDoc <> stepDoc <> Doc.text(")")
    })
    val commandsDoc = Doc.concat(commands.map(_.toDoc <> Doc.line))
    val bodyDoc = Doc.concat(body.map(_.toDoc <> Doc.line))
    Doc.text("(do (") <> varsDoc <> Doc.text(") (") <> test.toDoc <> commandsDoc <> Doc.text(")") <>
      Doc.indent(Doc.line <> bodyDoc) <> Doc.text(")")
  }
}

// Quotation
case class Quotation(value: Expression) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("'") <> value.toDoc
}

// Program
case class Program(
    expressions: List[Expression]
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = {
    Doc.concat(expressions.map(_.toDoc <> Doc.line))
  }
}
