package chester.error

import chester.i18n.*
import chester.syntax.Name
import chester.syntax.accociativity.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.utils.doc.*
import chester.utils.impls.*
import upickle.default.*

import scala.reflect.ClassTag

sealed trait TyckProblem extends Problem derives ReadWriter {
  final def stage: Problem.Stage = Problem.Stage.TYCK

  final def getMessage: String = {
    implicit val options: PrettierOptions = PrettierOptions.Default
    render(toDoc)
  }

  def hint: ToDoc = empty

  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc

  def cause: Term | Expr

  def location: Option[SourcePos] = cause match {
    case x: WithPos => x.sourcePos
    case _          => None
  }

  def renderWithLocation(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = {
    val baseMessage = Doc.text(t"Error") <+> this

    val locationInfo = location match {
      case Some(pos) =>
        val lines = pos.getLinesInRange match {
          case Some(lines) =>
            lines.map { case (lineNumber, line) =>
              Doc.text(t"$lineNumber") <+> Doc.text(line, Styling.BoldOn)
            }
          case None => Vector.empty
        }
        val locationHeader = Doc.text(t"Location") <+>
          Doc.text(
            t"${pos.fileName} [${pos.range.start.line + 1}:${pos.range.start.column.i + 1}] to [${pos.range.end.line + 1}:${pos.range.end.column.i + 1}]",
            Styling.BoldOn
          )

        val codeBlock = Doc.group(Doc.concat(lines.map(_.end)*))

        locationHeader <|> codeBlock

      case None =>
        val causeHeader = Doc.text(t"Cause", Styling.BoldOn)
        val causeText = cause
        causeHeader <|> causeText
    }

    baseMessage <|> locationInfo
  }
}

sealed trait TyckError extends TyckProblem derives ReadWriter {
  final override def severity: Problem.Severity = Problem.Severity.Error
}

sealed trait TyckWarning extends TyckProblem derives ReadWriter {
  final override def severity: Problem.Severity = Problem.Severity.Warning
}

case class UnusedVariableWarning(id: MaybeVarCall, cause: Expr) extends TyckWarning {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    d"Unused variable: ${id}"
}

implicit val rwThis: ReadWriter[QualifiedName | String] =
  union2RW[Expr, String](using
    implicitly[ClassTag[Expr]],
    implicitly[ClassTag[String]],
    a = qualifiedNameRW.asInstanceOf[ReadWriter[Expr]],
    b = readwriter[String]
  ).asInstanceOf

case class ExpectCase(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"case clause must have a pattern and a return expression"
}

case class ExpectFullCaseBlock(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Expected a full case block, got "
}

case class ExpectLambda(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Expected a lambda expression, got "
}

case class ExpectLetDef(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Expected a let or def statement, got "
}

case class UnexpectedTelescope(cause: MaybeTelescope) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Unexpected telescope"
}

case class ExpectParameterList(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Expected a parameter list, got ${cause}"
}

case class UnsupportedTermError(cause: Term) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Unsupported term"
}

sealed trait OpInfoError extends TyckError derives ReadWriter {
  override def cause: Term | Expr = EmptyExpr
}

case class UnknownOperator(override val cause: Expr) extends OpInfoError {
  override def toDoc(implicit options: PrettierOptions): Doc =
    t"Unknown operator."
}

case class PrecedenceCycleDetected(groups: Iterable[PrecedenceGroup]) extends OpInfoError {
  override def toDoc(implicit options: PrettierOptions): Doc =
    t"Precedence cycle detected among groups: ${groups.map(_.name).mkString(" -> ")}"
}

case class UnexpectedTokens(tokens: List[Expr]) extends OpInfoError {
  override def toDoc(implicit options: PrettierOptions): Doc =
    t"Unexpected tokens after parsing expression: $tokens"
}

case class UnknownPrecedenceGroup(group: PrecedenceGroup) extends OpInfoError {
  override def toDoc(implicit options: PrettierOptions): Doc =
    t"Unknown precedence group: '${group.name}'."
}

case class UnconnectedPrecedenceGroups(
    group1: PrecedenceGroup,
    group2: PrecedenceGroup
) extends OpInfoError {
  override def toDoc(implicit options: PrettierOptions): Doc =
    t"Precedence groups '${group1.name}' and '${group2.name}' are not connected."
}

case class UnboundVariable(name: Name, cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Unbound variable $name"
}

case class NotImplemented(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Not implemented ${cause.getClass.getName}"
}

case class TypeMismatch(lhs: Term, rhs: Term, cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = d"Type mismatch: expected $lhs but got $rhs"
}

case class DuplicateDefinition(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Duplicate definition"
}

case class FunctionCallUnificationError(
    functionType: Term,
    argumentTypes: Vector[Term],
    cause: Expr
) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Function call unification failed: expected $functionType but got $argumentTypes"

}

case class FunctionCallArityMismatchError(
    expected: Int,
    actual: Int,
    cause: Expr
) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Function expects $expected arguments, but got $actual"
}

case class FunctionCallArgumentMismatchError(
    expected: Int,
    actual: Int,
    cause: Expr
) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = t"Function expected $expected arguments, but received $actual"
}

case class ObjectFieldMismatch(
    missingInLHS: Seq[Term],
    missingInRHS: Seq[Term],
    cause: Expr
) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc = {
    val missingInLHSDoc = if (missingInLHS.nonEmpty) {
      Doc.text(t"Missing fields in LHS: ${missingInLHS.mkString(", ")}")
    } else Doc.empty
    val missingInRHSDoc = if (missingInRHS.nonEmpty) {
      Doc.text(t"Missing fields in RHS: ${missingInRHS.mkString(", ")}")
    } else Doc.empty
    missingInLHSDoc <> missingInRHSDoc
  }
}

case class InvalidImportSyntax(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Invalid syntax in import statement:"
}

case class InvalidModuleSyntax(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Invalid syntax in module statement:"
}

case class ExpectFieldDeclaration(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Expected a field declaration, got "
}

case class ExpectRecordName(cause: Expr) extends TyckError {
  override def toDoc(implicit
      options: PrettierOptions = PrettierOptions.Default
  ): Doc =
    t"Expected a record name, got "
}
case class DuplicateFieldDefinition(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc =
    t"Duplicate field definition in record"
}
case class UnsupportedExtendsType(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc =
    Doc.text("Unsupported type in extends clause")
}
case class ExpectTraitName(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc =
    t"Expected a trait name, got ${cause.toDoc}"
}

case class ExpectInterfaceName(cause: Expr) extends TyckError {
  override def toDoc(implicit options: PrettierOptions = PrettierOptions.Default): Doc =
    t"Expected an interface name, got ${cause.toDoc}"
}
