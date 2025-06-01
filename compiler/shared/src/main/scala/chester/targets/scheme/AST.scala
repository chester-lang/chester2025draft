package chester.targets.scheme

import chester.error.*
import chester.utils.doc.*
import chester.syntax.Tree
import chester.syntax.TreeMap
import upickle.default.*

case class Meta(span: Span) extends SpanRequired derives ReadWriter {
  inline def link(x: Doc): Doc = Doc.link(this, x)
}

extension (m: Option[Meta]) {
  def link(x: Doc): Doc = m match {
    case Some(m) => m.link(x)
    case None    => x
  }
}

// Base trait for all AST nodes
sealed trait ASTNode extends ToDoc with Tree[ASTNode] derives ReadWriter {
  val meta: Option[Meta]

  // Implementing the ThisTree type required by chester.syntax.Tree
  override type ThisTree <: ASTNode
}

// Expressions
sealed trait Expression extends ASTNode derives ReadWriter {
  override type ThisTree <: Expression
}

// Scheme literals
sealed trait Literal extends Expression derives ReadWriter {
  override type ThisTree <: Literal
}

case class StringLiteral(
    value: String,
    meta: Option[Meta] = None
) extends Literal {
  override type ThisTree = StringLiteral
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text(s"""$value"""))
  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = this
}

case class NumberLiteral(
    value: Double,
    meta: Option[Meta] = None
) extends Literal {
  override type ThisTree = NumberLiteral
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text(value.toString))
  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = this
}

case class BooleanLiteral(
    value: Boolean,
    meta: Option[Meta] = None
) extends Literal {
  override type ThisTree = BooleanLiteral
  def toDoc(using PrettierOptions): Doc =
    meta.link(if (value) Doc.text("#t") else Doc.text("#f"))
  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = this
}

case class NullLiteral(
    meta: Option[Meta] = None
) extends Literal {
  override type ThisTree = NullLiteral
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text("'()"))
  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = this
}

// Identifiers
case class Identifier(
    name: String,
    meta: Option[Meta] = None
) extends Expression {
  override type ThisTree = Identifier
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text(name))
  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = this
}

// ListExpression as a case class
case class ListExpression(
    elements: Vector[Expression],
    meta: Option[Meta] = None
) extends Expression derives ReadWriter {
  override type ThisTree = ListExpression

  def toDoc(using PrettierOptions): Doc = {
    val elemsDoc = Doc.sep(Doc.text(" "), elements.map(_.toDoc))
    meta.link(Doc.text("(") <> elemsDoc <> Doc.text("))"))
  }

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newElements = elements.map(e => g.use(e).asInstanceOf[Expression])
    if (elements eq newElements) this else ListExpression(newElements, meta)
  }
}

// DefineExpression function
def DefineExpression(
    name: Identifier,
    value: Expression,
    meta: Option[Meta] = None
): ListExpression =
  ListExpression(Vector(Identifier("define"), name, value), meta)

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
): ListExpression =
  ListExpression(Vector(Identifier("set!"), name, value), meta)

// BeginExpression function
def BeginExpression(
    expressions: Vector[Expression],
    meta: Option[Meta] = None
): ListExpression =
  ListExpression(Identifier("begin") +: expressions, meta)

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
  val elseExpr = elseClause.map(exprs => ListExpression(Identifier("else") +: exprs)).toSeq
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
  override type ThisTree = Quotation
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text("'") <> value.toDoc)

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newValue = g.use(value).asInstanceOf[Expression]
    if (value eq newValue) this else Quotation(newValue, meta)
  }
}

// QuasiQuotation class
case class QuasiQuotation(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  override type ThisTree = QuasiQuotation
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text("`") <> value.toDoc)

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newValue = g.use(value).asInstanceOf[Expression]
    if (value eq newValue) this else QuasiQuotation(newValue, meta)
  }
}

// Unquote class
case class Unquote(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  override type ThisTree = Unquote
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text(",") <> value.toDoc)

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newValue = g.use(value).asInstanceOf[Expression]
    if (value eq newValue) this else Unquote(newValue, meta)
  }
}

// UnquoteSplicing class
case class UnquoteSplicing(
    value: Expression,
    meta: Option[Meta] = None
) extends Expression {
  override type ThisTree = UnquoteSplicing
  def toDoc(using PrettierOptions): Doc = meta.link(Doc.text(",@") <> value.toDoc)

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newValue = g.use(value).asInstanceOf[Expression]
    if (value eq newValue) this else UnquoteSplicing(newValue, meta)
  }
}

// Program
case class Program(
    expressions: Vector[Expression],
    meta: Option[Meta] = None
) extends ASTNode {
  override type ThisTree = Program
  def toDoc(using PrettierOptions): Doc =
    meta.link(Doc.concat(expressions.map(_.toDoc <> Doc.line)))

  override def descent(f: ASTNode => ASTNode, g: TreeMap[ASTNode]): ASTNode = {
    val newExpressions = expressions.map(e => g.use(e).asInstanceOf[Expression])
    if (expressions eq newExpressions) this else Program(newExpressions, meta)
  }
}
