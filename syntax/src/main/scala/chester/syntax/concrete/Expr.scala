// TODO: Correctly implement toDoc. They are very broken
package chester.syntax.concrete

import cats.data.*
import chester.doc.*
import chester.doc.const.Docs
import chester.error.*
import chester.syntax.core.*
import chester.syntax.*
import chester.utils.doc.*
import upickle.default.*
import chester.utils.*
import spire.math.Rational
import chester.utils.impls.*
import chester.error.ProblemUpickle.*
import chester.uniqid.*
import chester.syntax.accociativity.Associativity

enum CommentType derives ReadWriter {
  case OneLine
  case MultiLine
}

case class Comment(
    content: String,
    typ: CommentType,
    sourcePos: Option[SourcePos]
) derives ReadWriter

case class CommentInfo(
    commentBefore: Vector[Comment],
    commentInBegin: Vector[Comment] = Vector.empty,
    commentInEnd: Vector[Comment] = Vector.empty,
    commentEndInThisLine: Vector[Comment] = Vector.empty
) derives ReadWriter {
  if (commentBefore.isEmpty && commentInBegin.isEmpty && commentInEnd.isEmpty && commentEndInThisLine.isEmpty) {
    throw new IllegalArgumentException("At least one comment should be present")
  }
}

case class ExprMeta(
    sourcePos: Option[SourcePos],
    commentInfo: Option[CommentInfo]
) derives ReadWriter {
  require(sourcePos.isDefined || commentInfo.isDefined)
}

object MetaFactory {
  def add(
      commentBefore: Vector[Comment] = Vector(),
      commentEndInThisLine: Vector[Comment] = Vector()
  )(updateOn: Option[ExprMeta]): Option[ExprMeta] =
    (commentBefore, commentEndInThisLine, updateOn) match {
      case (Vector(), Vector(), _) => updateOn
      case (before, end, Some(ExprMeta(sourcePos, None))) =>
        Some(
          ExprMeta(
            sourcePos,
            Some(
              CommentInfo(commentBefore = before, commentEndInThisLine = end)
            )
          )
        )
      case (before, end, Some(ExprMeta(sourcePos, Some(commentInfo)))) =>
        Some(
          ExprMeta(
            sourcePos,
            Some(
              commentInfo.copy(
                commentBefore = before ++ commentInfo.commentBefore,
                commentEndInThisLine = commentInfo.commentEndInThisLine ++ end
              )
            )
          )
        )
    }
}

sealed trait Expr extends WithPos with ToDoc derives ReadWriter {
  @deprecated("partially broken")
  def descent(operator: Expr => Expr): Expr = this
  def inspect(operator: Expr => Unit): Unit = {
    descent { x =>
      operator(x)
      x
    }
    ()
  }
  def foreach(operator: Expr => Unit): Unit = {
    inspect(_.foreach(operator))
    operator(this)
  }

  final def descentRecursive(operator: Expr => Expr): Expr = {
    operator(this.descent(_.descentRecursive(operator)))
  }

  // Shouldn't use this.type
  protected final inline def thisOr(inline x: Expr): this.type =
    reuse(this, x.asInstanceOf[this.type])

  /** Every Expr has meta to trace compile time errors type checking errors */
  def meta: Option[ExprMeta]
  final def sourcePos: Option[SourcePos] = meta.flatMap(_.sourcePos)

  def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr

  def commentAtStart(comment: Comment): Expr = updateMeta {
    case Some(meta) =>
      Some(
        meta.copy(commentInfo = meta.commentInfo.map(info => info.copy(commentBefore = info.commentBefore :+ comment)))
      )
    case None => Some(ExprMeta(None, Some(CommentInfo(Vector(comment)))))
  }

  def commentAtStart(comment: Vector[Comment]): Expr = if (comment.isEmpty) this
  else
    updateMeta {
      case Some(meta) =>
        Some(
          meta.copy(commentInfo = meta.commentInfo.map(info => info.copy(commentBefore = info.commentBefore ++ comment)))
        )
      case None => Some(ExprMeta(None, Some(CommentInfo(comment))))
    }

  def commentInfo: Option[CommentInfo] = meta.flatMap(_.commentInfo)

  override def toString: String = {
    implicit val options: PrettierOptions = PrettierOptions.Default
    render(this)
  }
}

sealed trait ParsedExpr extends Expr derives ReadWriter

sealed trait MaybeSaltedExpr extends Expr derives ReadWriter

case class Identifier(name: String, meta: Option[ExprMeta]) extends ParsedExpr derives ReadWriter {
  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None      => s"Identifier(\"${encodeString(name)}\")"
    case Some(pos) => s"Identifier(\"${encodeString(name)}\", ${pos.toString})"
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Identifier = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = Doc.text(name)

  def toSymbol: SymbolLiteral = SymbolLiteral(name, meta)
}

case class ResolvedIdentifier(
    module: QualifiedIDString,
    name: Name,
    meta: Option[ExprMeta]
) extends Expr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ResolvedIdentifier = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text(module.toString) <> Docs.`.` <> Doc.text(name.toString)
  )
}

case class ResolvedLocalVar(
    name: Name,
    varId: UniqId,
    meta: Option[ExprMeta]
) extends Expr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ResolvedLocalVar = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text(name.toString) <> Doc.text(s"(${varId})")
  )
}

case class OpSeq(seq: Vector[Expr], meta: Option[ExprMeta]) extends ParsedExpr with MaybeSaltedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    OpSeq(seq.map(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): OpSeq = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(seq.map(_.toDoc))
  )
}

case class InfixExpr(
    left: Expr,
    operator: Identifier,
    right: Expr,
    associativity: Associativity,
    meta: Option[ExprMeta]
) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    copy(left = left.descent(operator), right = right.descent(operator))
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): InfixExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    left.toDoc <+> operator.toDoc <+> right.toDoc
  )
}

case class PrefixExpr(
    operator: Identifier,
    operand: Expr,
    meta: Option[ExprMeta]
) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    copy(operand = operand.descent(operator))
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PrefixExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    operator.toDoc <+> operand.toDoc
  )
}

case class PostfixExpr(
    operand: Expr,
    operator: Identifier,
    meta: Option[ExprMeta]
) extends Expr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    copy(operand = operand.descent(operator))
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PostfixExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    operand.toDoc <+> operator.toDoc
  )
}
case class Block(
                  heads: Vector[Expr],
                  tail: Option[Expr],
                  meta: Option[ExprMeta]
) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Block = thisOr {
    Block(heads.map(operator), tail.map(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Block = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group {
    val headDocs = Doc.wrapperlist(Doc.empty, Doc.empty, Docs.`;` </> Doc.empty)(heads.map(_.toDoc))
    val tailDoc = tail.map(_.toDoc).getOrElse(Doc.empty)

    Docs.`{` </>
      Doc.indented(Doc.concat(headDocs, tailDoc)) </>
      Docs.`}`
  }
}

object Block {
  def apply(heads: Vector[Expr], tail: Expr): Block =
    Block(heads, Some(tail), None)

  def apply(heads: Vector[Expr], tail: Expr, meta: Option[ExprMeta]): Block =
    Block(heads, Some(tail), meta)
}

// In function declaration
case class Arg(
    decorations: Vector[Identifier] = Vector(),
    name: Identifier,
    ty: Option[Expr] = None,
    exprOrDefault: Option[Expr] = None,
    vararg: Boolean = false,
    meta: Option[ExprMeta]
) extends Expr derives ReadWriter {

  def getName: Name = name match {
    case Identifier(name, _) => name
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Arg = {
    Arg(
      decorations,
      name,
      ty.map(operator),
      exprOrDefault.map(operator),
      vararg,
      meta
    )
  }

  override def toString: String = this match {
    case Arg(decorations, name, ty, exorOrDefault, false, _) =>
      s"Arg($decorations,$name,$ty,$exorOrDefault)"
    case Arg(decorations, name, ty, exorOrDefault, vararg, _) =>
      s"Arg($decorations,$name,$ty,$exorOrDefault,$vararg)"
  }

  override def toDoc(using options: PrettierOptions): Doc = group {
    val decDoc =
      if (decorations.nonEmpty)
        Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(decorations.map(_.toDoc)) <+> Doc.empty
      else Doc.empty
    val nameDoc = name.toDoc
    val tyDoc = ty.map(t => Docs.`:` <+> t.toDoc).getOrElse(Doc.empty)
    val exprDoc =
      exprOrDefault.map(e => Docs.`=` <+> e.toDoc).getOrElse(Doc.empty)
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    decDoc <> nameDoc <> tyDoc <> exprDoc <> varargDoc
  }
}

case class CallingArg(
    name: Option[Identifier] = None,
    expr: Expr,
    vararg: Boolean = false,
    meta: Option[ExprMeta]
) extends ToDoc derives ReadWriter {
  def descent(operator: Expr => Expr): CallingArg = {
    CallingArg(name, operator(expr), vararg, meta)
  }

  override def toDoc(using options: PrettierOptions): Doc = group {
    val nameDoc = name.map(n => n.toDoc <> Docs.`=` <+> Doc.empty).getOrElse(Doc.empty)
    val exprDoc = expr.toDoc
    val varargDoc = if (vararg) Docs.`...` else Doc.empty
    nameDoc <> exprDoc <> varargDoc
  }
}

case class DesaltCallingTelescope(
    args: Vector[CallingArg],
    implicitly: Boolean = false,
    meta: Option[ExprMeta]
) extends MaybeTelescope
    with DesaltExpr {
  override def descent(operator: Expr => Expr): DesaltCallingTelescope =
    thisOr {
      DesaltCallingTelescope(args.map(_.descent(operator)), implicitly, meta)
    }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltCallingTelescope = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Doc.empty, Doc.empty, Docs.`,` <+> Doc.empty)(args.map(_.toDoc))
    if (implicitly) Docs.`[` <> argsDoc <> Docs.`]`
    else Docs.`(` <> argsDoc <> Docs.`)`
  }
}

sealed trait MaybeTelescope extends Expr derives ReadWriter {
  override def descent(operator: Expr => Expr): MaybeTelescope = thisOr(
    super.descent(operator).asInstanceOf[MaybeTelescope]
  )
}

sealed trait ParsedMaybeTelescope extends MaybeTelescope with ParsedExpr derives ReadWriter

case class Tuple(terms: Vector[Expr], meta: Option[ExprMeta]) extends ParsedMaybeTelescope {
  override def descent(operator: Expr => Expr): Tuple = thisOr {
    Tuple(terms.map(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Tuple = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,` <+> Doc.empty)(terms.map(_.toDoc))
}

case class DefTelescope(
    args: Vector[Arg],
    implicitly: Boolean = false,
    meta: Option[ExprMeta]
) extends MaybeTelescope
    with DesaltExpr derives ReadWriter {
  override def descent(operator: Expr => Expr): DefTelescope = thisOr {
    DefTelescope(args.map(_.descent(operator)), implicitly, meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DefTelescope = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,` <+> Doc.empty)(args.map(_.toDoc))
}

object DefTelescope {
  def of(args: Arg*)(using meta: Option[ExprMeta]): DefTelescope =
    DefTelescope(args.toVector, meta = meta)
}

case class FunctionCall(
    function: Expr,
    telescope: MaybeTelescope,
    meta: Option[ExprMeta]
) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    new FunctionCall(operator(function), telescope.descent(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): FunctionCall = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    function.toDoc <> telescope.toDoc
  )
}

case class DesaltFunctionCall(
    function: Expr,
    telescopes: Vector[DesaltCallingTelescope],
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override def descent(operator: Expr => Expr): DesaltFunctionCall = thisOr {
    DesaltFunctionCall(
      operator(function),
      telescopes.map(_.descent(operator)),
      meta
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltFunctionCall = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    function.toDoc <> Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescopes.map(_.toDoc))
  )
}

case class DotCall(
    expr: Expr,
    field: Expr,
    telescope: Vector[MaybeTelescope],
    meta: Option[ExprMeta]
) extends ParsedExpr derives ReadWriter {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    DotCall(
      operator(expr),
      operator(field),
      telescope.map(_.descent(operator)),
      meta
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DotCall = copy(meta = updater(meta))

  def isField: Boolean = telescope.isEmpty

  def isQualifiedName: Boolean = {
    if (telescope.nonEmpty) return false
    if (!field.isInstanceOf[Identifier]) return false
    expr match {
      case Identifier(_, _)    => true
      case DotCall(_, _, _, _) => expr.asInstanceOf[DotCall].isQualifiedName
      case _                   => false
    }
  }

  override def toDoc(using options: PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`.` <> field.toDoc <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

type QualifiedName = DotCall | Identifier
implicit val qualifiedNameRW: ReadWriter[QualifiedName] =
  union2RW[DotCall, Identifier]

object QualifiedName {
  def build(
      x: QualifiedName,
      field: Identifier,
      meta: Option[ExprMeta]
  ): QualifiedName = DotCall(x, field, Vector(), meta)
}

sealed trait Literal extends ParsedExpr derives ReadWriter

case class IntegerLiteral(value: BigInt, meta: Option[ExprMeta]) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString)
}

case class RationalLiteral(value: Rational, meta: Option[ExprMeta]) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text(value.toString)
}

case class StringLiteral(value: String, meta: Option[ExprMeta]) extends Literal {
  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None => s"StringLiteral(\"${encodeString(value)}\")"
    case Some(pos) =>
      s"StringLiteral(\"${encodeString(value)}\", ${pos.toString})"
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"")
}

case class SymbolLiteral(value: String, meta: Option[ExprMeta]) extends Literal {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("'" + value)

  def toIdentifier: Identifier = Identifier(value, meta)
}

case class ListExpr(terms: Vector[Expr], meta: Option[ExprMeta]) extends ParsedMaybeTelescope {
  override def descent(operator: Expr => Expr): ListExpr = thisOr {
    ListExpr(terms.map(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ListExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`, Docs.`,` <+> Doc.empty)(terms.map(_.toDoc))
}

case class HoleExpr(description: String, meta: Option[ExprMeta]) extends Expr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): HoleExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("?") <> Doc.text(description)
  )
}

case class TypeAnnotation(expr: Expr, ty: Expr, meta: Option[ExprMeta]) extends DesaltExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    TypeAnnotation(operator(expr), operator(ty), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): TypeAnnotation = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`:` <+> ty.toDoc
  )
}

case class AnnotatedExpr(
    annotation: Identifier,
    telescope: Vector[MaybeTelescope],
    expr: Expr,
    meta: Option[ExprMeta]
) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    AnnotatedExpr(
      annotation,
      telescope.map(_.descent(operator)),
      operator(expr),
      meta
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): AnnotatedExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    annotation.toDoc <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc)) <> expr.toDoc
  )
}

sealed trait ObjectClause derives ReadWriter {
  def descent(operator: Expr => Expr): ObjectClause = this match {
    case ObjectExprClause(k, v) => ObjectExprClause(k, operator(v))
    case ObjectExprClauseOnValue(k, v) =>
      ObjectExprClauseOnValue(operator(k), operator(v))
  }
}

case class ObjectExprClause(key: QualifiedName, value: Expr) extends ObjectClause {}

case class ObjectExprClauseOnValue(key: Expr, value: Expr) extends ObjectClause {}

object ObjectExprClause {
  def apply(key: QualifiedName, value: Expr): ObjectExprClause =
    new ObjectExprClause(key, value)
}

@deprecated
implicit def toObjectExprClause(pair: (QualifiedName, Expr)): ObjectExprClause =
  ObjectExprClause(pair._1, pair._2)

case class ObjectExpr(
    clauses: Vector[ObjectClause],
    meta: Option[ExprMeta]
) extends ParsedExpr
    with MaybeSaltedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    ObjectExpr(clauses.map(_.descent(operator)), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ObjectExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, Docs.`,` <+> Doc.empty)(
      clauses.map {
        case ObjectExprClause(key, value) =>
          key.toDoc <> Docs.`=` <+> value.toDoc
        case ObjectExprClauseOnValue(key, value) =>
          key.toDoc <> Docs.`=>` <+> value.toDoc
      }
    )
}

case class Keyword(
    key: Name,
    telescope: Vector[MaybeTelescope],
    meta: Option[ExprMeta]
) extends ParsedExpr {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    Keyword(key, telescope.map(_.descent(operator)), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Keyword = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("#" + key) <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

sealed trait DesaltExpr extends Expr derives ReadWriter

case class DesaltCaseClause(
    pattern: Expr,
    returning: Expr,
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltCaseClause = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): DesaltCaseClause = thisOr(
    DesaltCaseClause(operator(pattern), operator(returning), meta)
  )

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("case") <+> pattern.toDoc <+> Docs.`=>` <+> returning.toDoc
  )
}

case class DesaltMatching(
    clauses: Vector[DesaltCaseClause],
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltMatching = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    DesaltMatching(clauses.map(_.descent(operator)), meta)
  )

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`{`, Docs.`}`, Docs.`;` <+> Doc.empty)(
      clauses.map(_.toDoc)
    )
}

case class FunctionExpr(
    telescope: Vector[DefTelescope],
    resultTy: Option[Expr] = None,
    effect: Option[Expr] = None,
    body: Expr,
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): FunctionExpr = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    FunctionExpr(
      telescope.map(_.descent(operator)),
      resultTy.map(operator),
      effect.map(operator),
      operator(body),
      meta
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = group {
    val telescopeDoc = Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
    val effectDoc = effect.map(_.toDoc).getOrElse(Doc.empty)
    val resultDoc = resultTy.map(r => Docs.`->` <+> r.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.toDoc

    telescopeDoc <+> effectDoc <+> resultDoc <+> Docs.`{` </>
      Doc.indented(bodyDoc) </>
      Docs.`}`
  }
}

case class TypeAnotationNoEffects(
    expr: Expr,
    ty: Expr,
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): TypeAnotationNoEffects = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    TypeAnotationNoEffects(operator(expr), operator(ty), meta)
  )

  override def toDoc(using options: PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`:` <+> ty.toDoc
  )
}

sealed trait ErrorExpr extends Expr derives ReadWriter

case object EmptyExpr extends ErrorExpr {
  override def meta: Option[ExprMeta] = None

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): EmptyExpr.type = throw new UnsupportedOperationException()

  override def toDoc(using options: PrettierOptions): Doc =
    Doc.text("EmptyExpr")
}

case class DesaltFailed(
    origin: Expr,
    error: Problem,
    meta: Option[ExprMeta]
) extends ErrorExpr
    with Stmt {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltFailed = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("DesaltFailed(") <> origin.toDoc <> Doc.text(
      ", "
    ) <> error.toDoc <> Doc.text(")")
  )
}

sealed trait DesaltPattern extends DesaltExpr derives ReadWriter {
  override def descent(operator: Expr => Expr): DesaltPattern = this

  def bindings: Vector[Identifier] = Vector.empty
}

case class PatternCompare(literal: Expr, meta: Option[ExprMeta]) extends DesaltPattern {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PatternCompare = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = literal.toDoc
}

case class PatternBind(name: Identifier, meta: Option[ExprMeta]) extends DesaltPattern {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PatternBind = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = name.toDoc

  override def bindings: Vector[Identifier] = Vector(name)
}

case class Bindings(
    forwardingBindings: Vector[Identifier] = Vector.empty,
    sequentialBindings: Vector[Identifier] = Vector.empty
) {}

object Bindings {
  def reduce(bindings: Seq[Bindings]): Bindings = {
    val forwardingBindings = bindings.toVector.flatMap(_.forwardingBindings)
    val sequentialBindings = bindings.toVector.flatMap(_.sequentialBindings)
    Bindings(forwardingBindings, sequentialBindings)
  }
}

sealed trait Stmt extends DesaltExpr derives ReadWriter {
  def bindings: Bindings = Bindings(Vector.empty, Vector.empty)

}

case class ExprStmt(expr: Expr, meta: Option[ExprMeta]) extends Stmt {
  override def toDoc(using options: PrettierOptions): Doc = expr.toDoc

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

@deprecated("not used")
sealed trait PrecedenceGroupExpr

@deprecated("not used")
case class PrecedenceGroupResolving(
    name: Name,
    higherThan: Vector[UnresolvedID] = Vector(),
    lowerThan: Vector[UnresolvedID] = Vector(),
    associativity: Associativity = Associativity.None,
    meta: Option[ExprMeta]
) extends Stmt
    with PrecedenceGroupExpr {
  def getName: Option[Name] = Some(name)

  override def toDoc(using options: PrettierOptions): Doc = group {
    val nameDoc = name.toDoc
    val higherThanDoc =
      if (higherThan.isEmpty) Doc.empty
      else Doc.text("higher than ") <> higherThan.map(_.toString).mkString
    val lowerThanDoc =
      if (lowerThan.isEmpty) Doc.empty
      else Doc.text("lower than ") <> lowerThan.map(_.toString).mkString
    val associativityDoc = associativity match {
      case Associativity.None  => Doc.empty
      case Associativity.Left  => Doc.text("associativity left")
      case Associativity.Right => Doc.text("associativity right")
    }
    nameDoc <+> higherThanDoc <+> lowerThanDoc <+> associativityDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

@deprecated("not used")
case class PrecedenceGroupResolved(
    name: QualifiedIDString,
    higherThan: Vector[PrecedenceGroupResolved] = Vector(),
    lowerThan: Vector[PrecedenceGroupResolved] = Vector(),
    associativity: Associativity = Associativity.None,
    meta: Option[ExprMeta]
) extends Stmt
    with PrecedenceGroupExpr {
  def getName: Option[Name] = Some(name.name)

  override def toDoc(using options: PrettierOptions): Doc = group {
    val nameDoc = name.toString
    val higherThanDoc =
      if (higherThan.isEmpty) Doc.empty
      else Doc.text("higher than ") <> higherThan.map(_.toDoc).reduce(_ <+> _)
    val lowerThanDoc =
      if (lowerThan.isEmpty) Doc.empty
      else Doc.text("lower than ") <> lowerThan.map(_.toDoc).reduce(_ <+> _)
    val associativityDoc = associativity match {
      case Associativity.None  => Doc.empty
      case Associativity.Left  => Doc.text("associativity left")
      case Associativity.Right => Doc.text("associativity right")
    }
    nameDoc <+> higherThanDoc <+> lowerThanDoc <+> associativityDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

enum LetDefType derives ReadWriter {
  case Let
  case Def
}

sealed trait Defined extends ToDoc derives ReadWriter {
  def bindings: Vector[Identifier]
}

case class DefinedPattern(pattern: DesaltPattern) extends Defined {
  override def toDoc(using options: PrettierOptions): Doc = pattern.toDoc

  def bindings: Vector[Identifier] = pattern.bindings
}

case class DefinedFunction(
    id: Identifier,
    telescope: NonEmptyVector[DefTelescope]
) extends Defined {
  def bindings: Vector[Identifier] = Vector(id)

  override def toDoc(using options: PrettierOptions): Doc = group(
    id.toDoc <> Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

case class UnitExpr(meta: Option[ExprMeta]) extends DesaltExpr {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): UnitExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = Doc.text("()")

  override def descent(operator: Expr => Expr): Expr = this
}

case class LetDefStmt(
    kind: LetDefType,
    defined: Defined,
    body: Option[Expr] = None,
    ty: Option[Expr] = None,
    effect: Option[Expr] = None,
    decorations: Vector[Expr] = Vector(),
    meta: Option[ExprMeta]
) extends Stmt {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    LetDefStmt(
      kind,
      defined,
      body.map(operator),
      ty.map(operator),
      effect.map(operator),
      decorations.map(operator),
      meta
    )
  }

  override def bindings: Bindings = kind match {
    case LetDefType.Let => Bindings(sequentialBindings = defined.bindings)
    case LetDefType.Def => Bindings(forwardingBindings = defined.bindings)
  }

  override def toDoc(using options: PrettierOptions): Doc = group {
    val kindDoc = kind match {
      case LetDefType.Let => Doc.text("let")
      case LetDefType.Def => Doc.text("def")
    }
    val definedDoc = defined.toDoc
    val tyDoc = ty.map(t => Doc.text(": ") <> t.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(b => Doc.text(" = ") <> b.toDoc).getOrElse(Doc.empty)
    val decorationsDoc = Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(decorations.map(_.toDoc))
    decorationsDoc <+> kindDoc <+> definedDoc <+> tyDoc <+> bodyDoc
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

sealed trait DeclarationStmt extends Stmt derives ReadWriter

case class TraitStmt(
    name: Identifier,
    extendsClause: Option[ExtendsClause],
    body: Option[Block],
    meta: Option[ExprMeta]
) extends DeclarationStmt {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): TraitStmt =
    copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    copy(
      name = name,
      extendsClause = extendsClause.map(operator(_).asInstanceOf[ExtendsClause]),
      body = body.map(operator(_).asInstanceOf[Block]),
      meta = meta
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val nameDoc = name.toDoc
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("trait") <+> nameDoc <+> extendsDoc <+> bodyDoc
    )
  }
}

case class InterfaceStmt(
    name: Identifier,
    extendsClause: Option[ExtendsClause],
    body: Option[Block],
    meta: Option[ExprMeta]
) extends DeclarationStmt {
  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): InterfaceStmt =
    copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    copy(
      name = name,
      extendsClause = extendsClause.map(operator(_).asInstanceOf[ExtendsClause]),
      body = body.map(operator(_).asInstanceOf[Block]),
      meta = meta
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val nameDoc = name.toDoc
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("interface") <+> nameDoc <+> extendsDoc <+> bodyDoc
    )
  }
}

sealed trait Field extends DesaltExpr derives ReadWriter {
  def name: Identifier
  def ty: Option[Expr]
  override def descent(operator: Expr => Expr): Field = ???
}

case class RecordField(
    name: Identifier,
    ty: Option[Expr] = None,
    defaultValue: Option[Expr] = None,
    meta: Option[ExprMeta]
) extends Field {
  override def descent(operator: Expr => Expr): RecordField = copy(
    name = name,
    ty = ty.map(operator),
    defaultValue = defaultValue.map(operator),
    meta = meta
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): RecordField = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = {
    val tyDoc = ty.map(t => Docs.`:` <+> t.toDoc).getOrElse(Doc.empty)
    val defaultDoc = defaultValue.map(v => Docs.`=` <+> v.toDoc).getOrElse(Doc.empty)
    name.toDoc <> tyDoc <> defaultDoc
  }
}

case class ExtendsClause(superTypes: Vector[Expr], meta: Option[ExprMeta]) extends DesaltExpr {
  override def descent(operator: Expr => Expr): ExtendsClause = thisOr {
    ExtendsClause(superTypes.map(operator), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ExtendsClause = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = {
    val typesDoc = superTypes.map(_.toDoc).reduce((a, b) => a <+> Docs.`with` <+> b)
    Docs.`<:` <+> typesDoc
  }
}

case class RecordStmt(
    name: Identifier,
    fields: Vector[Field],
    extendsClause: Option[ExtendsClause],
    body: Option[Block],
    meta: Option[ExprMeta]
) extends DeclarationStmt {
  override def descent(operator: Expr => Expr): RecordStmt = copy(
    name = name,
    fields = fields.map(_.descent(operator)),
    extendsClause = extendsClause.map(operator(_).asInstanceOf[ExtendsClause]),
    body = body.map(_.descent(operator)),
    meta = meta
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): RecordStmt = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = {
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val fieldsDoc = Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,` <+> Doc.empty)(fields.map(_.toDoc))
    val bodyDoc = body.map(b => Doc.empty <+> b.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("record") <+> name.toDoc <> extendsDoc <> fieldsDoc <> bodyDoc
    )
  }
}
case class ObjectStmt(
    name: Identifier,
    extendsClause: Option[ExtendsClause],
    body: Option[Block],
    meta: Option[ExprMeta]
) extends DeclarationStmt {
  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ObjectStmt = copy(meta = updater(meta))

  override def descent(operator: Expr => Expr): Expr = thisOr(
    copy(
      name = name,
      extendsClause = extendsClause.map(operator(_).asInstanceOf[ExtendsClause]),
      body = body.map(operator(_).asInstanceOf[Block]),
      meta = meta
    )
  )

  override def toDoc(using options: PrettierOptions): Doc = {
    val nameDoc = name.toDoc
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("object") <+> nameDoc <+> extendsDoc <+> bodyDoc
    )
  }
}
case class ReturnStmt(expr: Expr, meta: Option[ExprMeta]) extends Stmt {
  override def descent(operator: Expr => Expr): Expr = thisOr {
    ReturnStmt(operator(expr), meta)
  }

  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("return ") <> expr.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

case class ImportStmt(module: ModuleRef, meta: Option[ExprMeta]) extends Stmt {
  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("import") <+> module.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

case class ModuleStmt(module: ModuleRef, meta: Option[ExprMeta]) extends Stmt {
  override def toDoc(using options: PrettierOptions): Doc = group(
    Doc.text("module") <+> module.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

case class BuiltinExpr(builtin: Builtin, meta: Option[ExprMeta]) extends Expr {
  override def descent(operator: Expr => Expr): Expr = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): BuiltinExpr = copy(meta = updater(meta))

  override def toDoc(using options: PrettierOptions): Doc = builtin.toDoc
}
