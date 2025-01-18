package chester.targets.scheme

import chester.error._
import chester.utils.doc._
import upickle.default._

case class Meta(sourcePos: SourcePos) derives ReadWriter {
  inline def link(x: Doc): Doc = Doc.link(this, x)
}

extension (m: Option[Meta]) {
  def link(x: Doc): Doc = m match {
    case Some(m) => m.link(x)
    case None    => x
  }
}

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
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text(s""""$value""""))
}

case class NumberLiteral(
    value: Double,
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text(value.toString))
}

case class BooleanLiteral(
    value: Boolean,
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc =
    meta.link(if (value) Doc.text("#t") else Doc.text("#f"))
}

case class NullLiteral(
    meta: Option[Meta] = None
) extends Literal {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text("'()"))
}

// Identifiers
case class Identifier(
    name: String,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text(name))
}

// ListExpression as a case class
case class ListExpression(
    elements: Vector[Expression],
    meta: Option[Meta] = None
) extends Expression derives ReadWriter {

  def toDoc(using options: PrettierOptions): Doc = {
    val elemsDoc = Doc.sep(Doc.text(" "), elements.map(_.toDoc))
    meta.link(Doc.text("(") <> elemsDoc <> Doc.text(")"))
  }
}

// DefineExpression function
def DefineExpression(
    name: Identifier,
    value: Expression,
    meta: Option[Meta] = None
): ListExpression = {
  ListExpression(Vector(Identifier("define"), name, value), meta)
}

// LambdaExpression function
def LambdaExpression(
    parameters: Vector[Identifier],
    body: Vector[Expression],
    meta: Option[Meta] = None
): ListExpression = {
  val paramsExpr = ListExpression(parameters.map(_.asInstanceOf[Expression]))
  ListExpression(Vector(Identifier("lambda"), paramsExpr) ++ body, meta)
}

// IfExpression function
def IfExpression(
    condition: Expression,
    thenBranch: Expression,
    elseBranch: Option[Expression],
    meta: Option[Meta] = None
): ListExpression = {
  val elsePart = elseBranch.toSeq
  ListExpression(Vector(Identifier("if"), condition, thenBranch) ++ elsePart, meta)
}

// SetExpression function
def SetExpression(
    name: Identifier,
    value: Expression,
    meta: Option[Meta] = None
): ListExpression = {
  ListExpression(Vector(Identifier("set!"), name, value), meta)
}

// BeginExpression function
def BeginExpression(
    expressions: Vector[Expression],
    meta: Option[Meta] = None
): ListExpression = {
  ListExpression(Identifier("begin") +: expressions, meta)
}

// LetExpression function
def LetExpression(
    bindings: Vector[(Identifier, Expression)],
    body: Vector[Expression],
    meta: Option[Meta] = None
): ListExpression = {
  val bindingsExpr = bindings.map { case (id, expr) =>
    ListExpression(Vector(id, expr))
  }
  ListExpression(Vector(Identifier("let"), ListExpression(bindingsExpr)) ++ body, meta)
}

// CondExpression function
def CondExpression(
    clauses: Vector[(Expression, Vector[Expression])],
    meta: Option[Meta] = None
): ListExpression = {
  val clausesExpr = clauses.map { case (test, exprs) =>
    ListExpression(test +: exprs)
  }
  ListExpression(Vector(Identifier("cond")) ++ clausesExpr, meta)
}

// CaseExpression function
def CaseExpression(
    key: Expression,
    clauses: Vector[(Vector[Literal], Vector[Expression])],
    elseClause: Option[Vector[Expression]] = None,
    meta: Option[Meta] = None
): ListExpression = {
  val clausesExpr = clauses.map { case (datums, exprs) =>
    val datumsExpr = ListExpression(datums.map(_.asInstanceOf[Expression]))
    ListExpression(datumsExpr +: exprs)
  }
  val elseExpr = elseClause.map { exprs =>
    ListExpression(Identifier("else") +: exprs)
  }.toSeq
  ListExpression(Vector(Identifier("case"), key) ++ clausesExpr ++ elseExpr, meta)
}

// DoExpression function
def DoExpression(
    variables: Vector[(Identifier, Expression, Option[Expression])],
    test: Expression,
    commands: Vector[Expression],
    body: Vector[Expression],
    meta: Option[Meta] = None
): ListExpression = {
  val varsExpr = variables.map { case (id, init, stepOpt) =>
    val varList = Vector(id, init) ++ stepOpt.toSeq
    ListExpression(varList)
  }
  val testExpr = ListExpression(Vector(test) ++ commands)
  ListExpression(Vector(Identifier("do"), ListExpression(varsExpr), testExpr) ++ body, meta)
}

// Quotation
case class Quotation(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text("'") <> value.toDoc)
}

// QuasiQuotation class
case class QuasiQuotation(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text("`") <> value.toDoc)
}

// Unquote class
case class Unquote(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text(",") <> value.toDoc)
}

// UnquoteSplicing class
case class UnquoteSplicing(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  def toDoc(using options: PrettierOptions): Doc = meta.link(Doc.text(",@") <> value.toDoc)
}

// Program
case class Program(
    expressions: Vector[Expression],
    meta: Option[Meta] = None
) extends ASTNode {
  def toDoc(using options: PrettierOptions): Doc = {
    meta.link(Doc.concat(expressions.map(_.toDoc <> Doc.line)))
  }
}
