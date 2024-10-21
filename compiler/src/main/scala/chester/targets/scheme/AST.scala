package chester.targets.scheme

import chester.error.*
import chester.utils.doc.*
import upickle.default.*

case class Meta(sourcePos: SourcePos) derives ReadWriter

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc derives ReadWriter {
  val meta: Option[Meta]
}

// Expressions
sealed trait Expression extends ASTNode derives ReadWriter

// Scheme literals
sealed trait Literal extends Expression derives ReadWriter

case class StringLiteral(
    value: String,
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(s""""$value"""")
}

case class NumberLiteral(
    value: Double,
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(value.toString)
}

case class BooleanLiteral(
    value: Boolean,
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc =
    if (value) Doc.text("#t") else Doc.text("#f")
}

case class NullLiteral(
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("'()")
}

// Identifiers
case class Identifier(
    name: String,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = Doc.text(name)
}

// List expression trait
sealed trait ListExpression extends Expression derives ReadWriter {
  val meta: Option[Meta]
  def elements: Seq[Expression]

  def toDoc(using options: PrettierOptions): Doc = {
    val elemsDoc = Doc.sep(Doc.text(" "), elements.map(_.toDoc))
    Doc.text("(") <> elemsDoc <> Doc.text(")")
  }
}

// Generic list expression (for general lists)
case class GenericListExpression(
    elements: Seq[Expression],
    meta: Option[Meta] = None
) extends ListExpression

// Define expression
case class DefineExpression(
    name: Identifier,
    value: Expression,
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = Seq(Identifier("define"), name, value)
}

// Lambda expression
case class LambdaExpression(
    parameters: List[Identifier],
    body: List[Expression],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    val paramsExpr = GenericListExpression(parameters)
    Seq(Identifier("lambda"), paramsExpr) ++ body
  }
}

// If expression
case class IfExpression(
    condition: Expression,
    thenBranch: Expression,
    elseBranch: Option[Expression],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    Seq(Identifier("if"), condition, thenBranch) ++ elseBranch.toSeq
  }
}

// Set! expression
case class SetExpression(
    name: Identifier,
    value: Expression,
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = Seq(Identifier("set!"), name, value)
}

// Begin expression
case class BeginExpression(
    expressions: List[Expression],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = Identifier("begin") +: expressions
}

// Let expression
case class LetExpression(
    bindings: List[(Identifier, Expression)],
    body: List[Expression],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    val bindingsExpr = bindings.map { case (id, expr) =>
      GenericListExpression(List(id, expr))
    }
    Seq(Identifier("let"), GenericListExpression(bindingsExpr)) ++ body
  }
}

// Cond expression
case class CondExpression(
    clauses: List[(Expression, List[Expression])],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    val clausesExpr = clauses.map { case (test, exprs) =>
      GenericListExpression(test +: exprs)
    }
    Seq(Identifier("cond")) ++ clausesExpr
  }
}

// Case expression
case class CaseExpression(
    key: Expression,
    clauses: List[(List[Literal], List[Expression])],
    elseClause: Option[List[Expression]] = None,
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    val clausesExpr = clauses.map { case (datums, exprs) =>
      val datumsExpr = GenericListExpression(datums)
      GenericListExpression(datumsExpr +: exprs)
    }
    val elseExpr = elseClause.map { exprs =>
      GenericListExpression(Identifier("else") +: exprs)
    }.toSeq
    Seq(Identifier("case"), key) ++ clausesExpr ++ elseExpr
  }
}

// Do expression
case class DoExpression(
    variables: List[(Identifier, Expression, Option[Expression])],
    test: Expression,
    commands: List[Expression],
    body: List[Expression],
    meta: Option[Meta] = None
) extends ListExpression {
  def elements: Seq[Expression] = {
    val varsExpr = variables.map { case (id, init, stepOpt) =>
      val varList = Seq(id, init) ++ stepOpt.toSeq
      GenericListExpression(varList)
    }
    val testExpr = GenericListExpression(Seq(test) ++ commands)
    Seq(Identifier("do"), GenericListExpression(varsExpr), testExpr) ++ body
  }
}

// Quotation
case class Quotation(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = Doc.text("'") <> value.toDoc
}

// Program
case class Program(
    expressions: List[Expression],
    meta: Option[Meta] = None
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = {
    Doc.concat(expressions.map(_.toDoc <> Doc.line))
  }
}
