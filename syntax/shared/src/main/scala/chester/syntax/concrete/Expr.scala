// TODO: Correctly implement toDoc. They are very broken
package chester.syntax.concrete

import cats.data.*
import chester.doc.*
import chester.doc.consts.Docs
import chester.error.*
import chester.syntax.*
import chester.utils.doc.*
import upickle.default.*
import chester.utils.*
import spire.math.Rational
import chester.utils.impls.*
import chester.error.ProblemUpickle.*
import chester.uniqid.*
import chester.syntax.accociativity.Associativity

import scala.language.implicitConversions

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

object CommentInfo {
  def maybe(
      commentBefore: Vector[Comment],
      commentInBegin: Vector[Comment] = Vector.empty,
      commentInEnd: Vector[Comment] = Vector.empty,
      commentEndInThisLine: Vector[Comment] = Vector.empty
  ): Option[CommentInfo] =
    Option.unless(commentBefore.isEmpty && commentInBegin.isEmpty && commentInEnd.isEmpty && commentEndInThisLine.isEmpty)(CommentInfo(commentBefore, commentInBegin, commentInEnd, commentEndInThisLine))
}

case class ExprMeta(
    sourcePos: Option[SourcePos],
    commentInfo: Option[CommentInfo]
) derives ReadWriter {
  require(sourcePos.isDefined || commentInfo.isDefined)
}

object ExprMeta {
  def maybe(sourcePos: Option[SourcePos] = None, commentInfo: Option[CommentInfo] = None): Option[ExprMeta] =
    Option.unless(sourcePos.isEmpty && commentInfo.isEmpty)(ExprMeta(sourcePos, commentInfo))
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
      case _ => unreachable()
    }
}

sealed trait Expr extends WithPos with Tree[Expr] with ToDoc derives ReadWriter {
  type ThisTree <: Expr

  def descent(f: Expr => Expr, g: TreeMap[Expr]): Expr

  /** Every Expr has meta to trace compile time errors, type checking errors */
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

sealed trait ParsedExpr extends Expr derives ReadWriter {
  override type ThisTree <: ParsedExpr
}

sealed trait MaybeSaltedExpr extends Expr derives ReadWriter {
  override type ThisTree <: MaybeSaltedExpr
}

case class Identifier(name: String, meta: Option[ExprMeta]) extends ParsedExpr derives ReadWriter {
  override type ThisTree = Identifier

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Identifier = this

  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None      => s"Identifier(\"${encodeString(name)}\")"
    case Some(pos) => s"Identifier(\"${encodeString(name)}\", $pos)"
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Identifier = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = Doc.text(name)

  def toSymbol: SymbolLiteral = SymbolLiteral(name, meta)
}

@deprecated("not used")
case class ResolvedIdentifier(
    module: QualifiedIDString,
    name: Name,
    meta: Option[ExprMeta]
) extends Expr {
  override type ThisTree = ResolvedIdentifier

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ResolvedIdentifier = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ResolvedIdentifier = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text(module.toString) <> Docs.`.` <> Doc.text(name.toString)
  )
}

@deprecated("not used")
case class ResolvedLocalVar(
    name: Name,
    varId: Uniqid,
    meta: Option[ExprMeta]
) extends Expr {
  override type ThisTree = ResolvedLocalVar

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ResolvedLocalVar = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ResolvedLocalVar = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text(name.toString) <> Doc.text(s"($varId)")
  )
}

case class OpSeq(seq: Vector[Expr], meta: Option[ExprMeta]) extends ParsedExpr with MaybeSaltedExpr {
  override type ThisTree = OpSeq

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): OpSeq = thisOr {
    OpSeq(seq.map(f), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): OpSeq = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
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
  override type ThisTree = InfixExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): InfixExpr = thisOr {
    copy(
      left = f(left),
      operator = g.use(operator),
      right = f(right)
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): InfixExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    left.toDoc <+> operator.toDoc <+> right.toDoc
  )
}

case class PrefixExpr(
    operator: Identifier,
    operand: Expr,
    meta: Option[ExprMeta]
) extends Expr {
  override type ThisTree = PrefixExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PrefixExpr = thisOr {
    copy(
      operator = g.use(operator),
      operand = f(operand)
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PrefixExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    operator.toDoc <+> operand.toDoc
  )
}

case class PostfixExpr(
    operand: Expr,
    operator: Identifier,
    meta: Option[ExprMeta]
) extends Expr {
  override type ThisTree = PostfixExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PostfixExpr = thisOr {
    copy(
      operand = f(operand),
      operator = g.use(operator)
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PostfixExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    operand.toDoc <+> operator.toDoc
  )
}

case class Block(
    statements: Vector[Expr],
    result: Option[Expr],
    meta: Option[ExprMeta]
) extends ParsedExpr {
  override type ThisTree = Block

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Block = thisOr {
    Block(statements.map(f), result.map(f), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Block = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group {
    val headDocs = Doc.wrapperlist(Doc.empty, Doc.empty, Docs.`;` </> Doc.empty)(statements.map(_.toDoc))
    val tailDoc = result.map(_.toDoc).getOrElse(Doc.empty)

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
  override type ThisTree = Arg

  def getName: Name = name match {
    case Identifier(name, _) => name
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Arg =
    copy(meta = updater(meta))

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Arg =
    Arg(
      decorations.map(g.use),
      g.use(name),
      ty.map(f),
      exprOrDefault.map(f),
      vararg,
      meta
    )

  override def toString: String = this match {
    case Arg(decorations, name, ty, exorOrDefault, false, _) =>
      s"Arg($decorations,$name,$ty,$exorOrDefault)"
    case Arg(decorations, name, ty, exorOrDefault, vararg, _) =>
      s"Arg($decorations,$name,$ty,$exorOrDefault,$vararg)"
  }

  override def toDoc(using PrettierOptions): Doc = group {
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
) extends Expr derives ReadWriter {
  override type ThisTree = CallingArg

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): CallingArg =
    copy(meta = updater(meta))

  def descent(f: Expr => Expr, g: TreeMap[Expr]): CallingArg =
    CallingArg(name.map(g.use), f(expr), vararg, meta)

  override def toDoc(using PrettierOptions): Doc = group {
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
  override type ThisTree = DesaltCallingTelescope

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltCallingTelescope =
    thisOr {
      DesaltCallingTelescope(args.map(_.descent(f, g)), implicitly, meta)
    }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltCallingTelescope = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
    val argsDoc = Doc.wrapperlist(Doc.empty, Doc.empty, Docs.`,` <+> Doc.empty)(args.map(_.toDoc))
    if (implicitly) Docs.`[` <> argsDoc <> Docs.`]`
    else Docs.`(` <> argsDoc <> Docs.`)`
  }
}

sealed trait MaybeTelescope extends Expr derives ReadWriter {
  override type ThisTree <: MaybeTelescope

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): MaybeTelescope = this
}

sealed trait ParsedMaybeTelescope extends MaybeTelescope with ParsedExpr derives ReadWriter {
  override type ThisTree <: ParsedMaybeTelescope
}

case class Tuple(terms: Vector[Expr], meta: Option[ExprMeta]) extends ParsedMaybeTelescope {
  override type ThisTree = Tuple

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Tuple = thisOr {
    Tuple(terms.map(f), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Tuple = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`(`, Docs.`)`, Docs.`,` <+> Doc.empty)(terms.map(_.toDoc))
}

case class DefTelescope(
    args: Vector[Arg],
    implicitly: Boolean = false,
    meta: Option[ExprMeta]
) extends MaybeTelescope
    with DesaltExpr derives ReadWriter {
  override type ThisTree = DefTelescope

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DefTelescope = thisOr {
    DefTelescope(args.map(_.descent(f, g)), implicitly, meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DefTelescope = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
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
  override type ThisTree = FunctionCall

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): FunctionCall = thisOr {
    new FunctionCall(f(function), telescope.descent(f, g), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): FunctionCall = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    function.toDoc <> telescope.toDoc
  )
}

case class DesaltFunctionCall(
    function: Expr,
    telescopes: Vector[DesaltCallingTelescope],
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override type ThisTree = DesaltFunctionCall

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltFunctionCall = thisOr {
    DesaltFunctionCall(
      f(function),
      telescopes.map(_.descent(f, g)),
      meta
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltFunctionCall = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    function.toDoc <> Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescopes.map(_.toDoc))
  )
}

case class DotCall(
    expr: Expr,
    field: Expr,
    telescope: Vector[MaybeTelescope],
    meta: Option[ExprMeta]
) extends ParsedExpr derives ReadWriter {
  override type ThisTree = DotCall

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DotCall = thisOr {
    DotCall(
      f(expr),
      f(field),
      telescope.map(_.descent(f, g)),
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
      case _: Identifier => true
      case dc: DotCall   => dc.isQualifiedName
      case _             => false
    }
  }

  override def toDoc(using PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`.` <> field.toDoc <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

type QualifiedName = DotCall | Identifier
implicit val qualifiedNameRW: ReadWriter[QualifiedName] =
  ReadWriter.merge(upickle.default.macroRW[DotCall], upickle.default.macroRW[Identifier])

object QualifiedName {
  def build(
      x: QualifiedName,
      field: Identifier,
      meta: Option[ExprMeta]
  ): QualifiedName = DotCall(x, field, Vector(), meta)
}

sealed trait Literal extends ParsedExpr derives ReadWriter {
  override type ThisTree <: Literal
}

case class IntegerLiteral(value: BigInt, meta: Option[ExprMeta]) extends Literal {
  override type ThisTree = IntegerLiteral

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): IntegerLiteral = this

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString)
}

case class RationalLiteral(value: Rational, meta: Option[ExprMeta]) extends Literal {
  override type ThisTree = RationalLiteral

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): RationalLiteral = this

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.text(value.toString)
}

case class StringLiteral(value: String, meta: Option[ExprMeta]) extends Literal {
  override type ThisTree = StringLiteral

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): StringLiteral = this

  override def toString: String = meta.flatMap(_.sourcePos) match {
    case None => s"StringLiteral(\"${encodeString(value)}\")"
    case Some(pos) =>
      s"StringLiteral(\"${encodeString(value)}\", $pos)"
  }

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.text("\"" + encodeString(value) + "\"")
}

case class SymbolLiteral(value: String, meta: Option[ExprMeta]) extends Literal {
  override type ThisTree = SymbolLiteral

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): SymbolLiteral = this

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.text("'" + value)

  def toIdentifier: Identifier = Identifier(value, meta)
}

case class ListExpr(terms: Vector[Expr], meta: Option[ExprMeta]) extends ParsedMaybeTelescope {
  override type ThisTree = ListExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ListExpr = thisOr {
    ListExpr(terms.map(f), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ListExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
    Doc.wrapperlist(Docs.`[`, Docs.`]`, Docs.`,` <+> Doc.empty)(terms.map(_.toDoc))
}

case class HoleExpr(description: String, meta: Option[ExprMeta]) extends Expr {
  override type ThisTree = HoleExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): HoleExpr = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): HoleExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("?") <> Doc.text(description)
  )
}

case class TypeAnnotation(expr: Expr, ty: Expr, meta: Option[ExprMeta]) extends DesaltExpr {
  override type ThisTree = TypeAnnotation

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): TypeAnnotation = thisOr {
    TypeAnnotation(f(expr), f(ty), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): TypeAnnotation = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`:` <+> ty.toDoc
  )
}

case class AnnotatedExpr(
    annotation: Identifier,
    telescope: Vector[MaybeTelescope],
    expr: Expr,
    meta: Option[ExprMeta]
) extends ParsedExpr {
  override type ThisTree = AnnotatedExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): AnnotatedExpr = thisOr {
    AnnotatedExpr(
      g.use(annotation),
      telescope.map(_.descent(f, g)),
      f(expr),
      meta
    )
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): AnnotatedExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    annotation.toDoc <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc)) <> expr.toDoc
  )
}

sealed trait ObjectClause extends Product with Serializable derives ReadWriter {
  def descent(f: Expr => Expr, g: TreeMap[Expr]): ObjectClause
}

case class ObjectExprClause(key: QualifiedName, value: Expr) extends ObjectClause {
  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ObjectExprClause =
    ObjectExprClause(key, f(value))
}

case class ObjectExprClauseOnValue(key: Expr, value: Expr) extends ObjectClause {
  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ObjectExprClauseOnValue =
    ObjectExprClauseOnValue(f(key), f(value))
}

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
  override type ThisTree = ObjectExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ObjectExpr = thisOr {
    ObjectExpr(clauses.map(_.descent(f, g)), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ObjectExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
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
  override type ThisTree = Keyword

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Keyword = thisOr {
    Keyword(key, telescope.map(_.descent(f, g)), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): Keyword = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("#" + key) <>
      Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

sealed trait DesaltExpr extends Expr derives ReadWriter {
  override type ThisTree <: DesaltExpr
}

case class DesaltCaseClause(
    pattern: Expr,
    returning: Expr,
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override type ThisTree = DesaltCaseClause

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltCaseClause = thisOr(
    DesaltCaseClause(f(pattern), f(returning), meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltCaseClause = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("case") <+> pattern.toDoc <+> Docs.`=>` <+> returning.toDoc
  )
}

case class DesaltMatching(
    clauses: Vector[DesaltCaseClause],
    meta: Option[ExprMeta]
) extends DesaltExpr {
  override type ThisTree = DesaltMatching

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltMatching = thisOr(
    DesaltMatching(clauses.map(_.descent(f, g)), meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltMatching = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc =
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
  override type ThisTree = FunctionExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): FunctionExpr = thisOr(
    FunctionExpr(
      telescope.map(_.descent(f, g)),
      resultTy.map(f),
      effect.map(f),
      f(body),
      meta
    )
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): FunctionExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group {
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
  override type ThisTree = TypeAnotationNoEffects

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): TypeAnotationNoEffects = thisOr(
    TypeAnotationNoEffects(f(expr), f(ty), meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): TypeAnotationNoEffects = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    expr.toDoc <> Docs.`:` <+> ty.toDoc
  )
}

sealed trait ErrorExpr extends Expr derives ReadWriter {
  override type ThisTree <: ErrorExpr
}

case class RecoverableParseError(
    partialResult: Option[Expr],
    message: String,
    pos: Pos,
    meta: Option[ExprMeta]
) extends ErrorExpr {
  override type ThisTree = RecoverableParseError

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): RecoverableParseError = thisOr(
    RecoverableParseError(partialResult.map(f), message, pos, meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): RecoverableParseError = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("RecoverableParseError(") <>
      partialResult.map(_.toDoc).getOrElse(Doc.text("None")) <> Doc.text(", ") <>
      Doc.text(message) <> Doc.text(")")
  )
}

case object EmptyExpr extends ErrorExpr {
  override type ThisTree = EmptyExpr.type

  override def meta: Option[ExprMeta] = None

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): EmptyExpr.type = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): EmptyExpr.type = throw new UnsupportedOperationException()

  override def toDoc(using PrettierOptions): Doc =
    Doc.text("EmptyExpr")
}

case class DesaltFailed(
    origin: Expr,
    error: Problem,
    meta: Option[ExprMeta]
) extends ErrorExpr
    with Stmt {
  override type ThisTree = DesaltFailed

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltFailed = thisOr(
    DesaltFailed(f(origin), error, meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): DesaltFailed = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("DesaltFailed(") <> origin.toDoc <> Doc.text(
      ", "
    ) <> error.toDoc <> Doc.text(")")
  )
}

sealed trait DesaltPattern extends DesaltExpr derives ReadWriter {
  override type ThisTree <: DesaltPattern

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): DesaltPattern = this

  def bindings: Vector[Identifier] = Vector.empty
}

case class PatternCompare(literal: Expr, meta: Option[ExprMeta]) extends DesaltPattern {
  override type ThisTree = PatternCompare

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PatternCompare = thisOr(
    PatternCompare(f(literal), meta)
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PatternCompare = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = literal.toDoc
}

case class PatternBind(name: Identifier, meta: Option[ExprMeta]) extends DesaltPattern {
  override type ThisTree = PatternBind

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PatternBind = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): PatternBind = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = name.toDoc

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
  override type ThisTree <: Stmt
  def bindings: Bindings = Bindings(Vector.empty, Vector.empty)
}

case class ExprStmt(expr: Expr, meta: Option[ExprMeta]) extends Stmt {
  override type ThisTree = ExprStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ExprStmt = thisOr(
    ExprStmt(f(expr), meta)
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): ExprStmt =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = expr.toDoc
}

@deprecated("not used")
sealed trait PrecedenceGroupExpr extends Product with Serializable

@deprecated("not used")
case class PrecedenceGroupResolving(
    name: Name,
    higherThan: Vector[UnresolvedID] = Vector(),
    lowerThan: Vector[UnresolvedID] = Vector(),
    associativity: Associativity = Associativity.None,
    meta: Option[ExprMeta]
) extends Stmt
    with PrecedenceGroupExpr {
  override type ThisTree = PrecedenceGroupResolving

  def getName: Option[Name] = Some(name)

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PrecedenceGroupResolving = this

  override def toDoc(using PrettierOptions): Doc = group {
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
      case _                   => unreachable()
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
  override type ThisTree = PrecedenceGroupResolved

  def getName: Option[Name] = Some(name.name)

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): PrecedenceGroupResolved = this

  override def toDoc(using PrettierOptions): Doc = group {
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
      case _                   => unreachable()
    }
    Doc.text(nameDoc) <+> higherThanDoc <+> lowerThanDoc <+> associativityDoc
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
  override def toDoc(using PrettierOptions): Doc = pattern.toDoc

  def bindings: Vector[Identifier] = pattern.bindings
}

case class DefinedFunction(
    id: Identifier,
    telescope: NonEmptyVector[DefTelescope]
) extends Defined {
  def bindings: Vector[Identifier] = Vector(id)

  override def toDoc(using PrettierOptions): Doc = group(
    id.toDoc <> Doc.wrapperlist(Doc.empty, Doc.empty, Doc.empty)(telescope.map(_.toDoc))
  )
}

case class UnitExpr(meta: Option[ExprMeta]) extends DesaltExpr {
  override type ThisTree = UnitExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): UnitExpr = this

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): UnitExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = Doc.text("()")
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
  override type ThisTree = LetDefStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): LetDefStmt = thisOr {
    LetDefStmt(
      kind,
      defined, // Assuming Defined does not need descent
      body.map(f),
      ty.map(f),
      effect.map(f),
      decorations.map(f),
      meta
    )
  }

  override def bindings: Bindings = kind match {
    case LetDefType.Let => Bindings(sequentialBindings = defined.bindings)
    case LetDefType.Def => Bindings(forwardingBindings = defined.bindings)
  }

  override def toDoc(using PrettierOptions): Doc = group {
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

sealed trait DeclarationStmt extends Stmt derives ReadWriter {
  override type ThisTree <: DeclarationStmt
}

case class TraitStmt(
    name: Identifier,
    extendsClause: Option[ExtendsClause],
    body: Option[Block],
    meta: Option[ExprMeta]
) extends DeclarationStmt {
  override type ThisTree = TraitStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Expr = thisOr(
    copy(
      name = g.use(name),
      extendsClause = extendsClause.map(_.descent(f, g)),
      body = body.map(_.descent(f, g)),
      meta = meta
    )
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): TraitStmt =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
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
  override type ThisTree = InterfaceStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Expr = thisOr(
    copy(
      name = g.use(name),
      extendsClause = extendsClause.map(_.descent(f, g)),
      body = body.map(_.descent(f, g)),
      meta = meta
    )
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): InterfaceStmt =
    copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
    val nameDoc = name.toDoc
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("interface") <+> nameDoc <+> extendsDoc <+> bodyDoc
    )
  }
}

sealed trait Field extends DesaltExpr derives ReadWriter {
  override type ThisTree <: Field

  def name: Identifier
  def ty: Option[Expr]
  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Field
}

case class RecordField(
    name: Identifier,
    ty: Option[Expr] = None,
    defaultValue: Option[Expr] = None,
    meta: Option[ExprMeta]
) extends Field {
  override type ThisTree = RecordField

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): RecordField = copy(
    name = g.use(name),
    ty = ty.map(f),
    defaultValue = defaultValue.map(f),
    meta = meta
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): RecordField = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
    val tyDoc = ty.map(t => Docs.`:` <+> t.toDoc).getOrElse(Doc.empty)
    val defaultDoc = defaultValue.map(v => Docs.`=` <+> v.toDoc).getOrElse(Doc.empty)
    name.toDoc <> tyDoc <> defaultDoc
  }
}

case class ExtendsClause(superTypes: Vector[Expr], meta: Option[ExprMeta]) extends DesaltExpr {
  override type ThisTree = ExtendsClause

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ExtendsClause = thisOr {
    ExtendsClause(superTypes.map(f), meta)
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ExtendsClause = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
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
  override type ThisTree = RecordStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): RecordStmt = copy(
    name = g.use(name),
    fields = fields.map(_.descent(f, g)),
    extendsClause = extendsClause.map(_.descent(f, g)),
    body = body.map(_.descent(f, g)),
    meta = meta
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): RecordStmt = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
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
  override type ThisTree = ObjectStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): Expr = thisOr(
    copy(
      name = g.use(name),
      extendsClause = extendsClause.map(_.descent(f, g)),
      body = body.map(_.descent(f, g)),
      meta = meta
    )
  )

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): ObjectStmt = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = {
    val nameDoc = name.toDoc
    val extendsDoc = extendsClause.map(_.toDoc).getOrElse(Doc.empty)
    val bodyDoc = body.map(_.toDoc).getOrElse(Doc.empty)
    group(
      Doc.text("object") <+> nameDoc <+> extendsDoc <+> bodyDoc
    )
  }
}

case class ReturnStmt(expr: Expr, meta: Option[ExprMeta]) extends Stmt {
  override type ThisTree = ReturnStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ReturnStmt = thisOr {
    ReturnStmt(f(expr), meta)
  }

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("return ") <> expr.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

case class ImportStmt(module: ModuleRef, meta: Option[ExprMeta]) extends Stmt {
  override type ThisTree = ImportStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ImportStmt = this

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("import") <+> module.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

case class ModuleStmt(module: ModuleRef, meta: Option[ExprMeta]) extends Stmt {
  override type ThisTree = ModuleStmt

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): ModuleStmt = this

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.text("module") <+> module.toDoc
  )

  override def updateMeta(updater: Option[ExprMeta] => Option[ExprMeta]): Expr =
    copy(meta = updater(meta))
}

// Define an expression representing a union type (A | B | C)
case class UnionTypeExpr(
    types: Vector[Expr],
    meta: Option[ExprMeta]
) extends Expr {
  override type ThisTree = UnionTypeExpr

  override def descent(f: Expr => Expr, g: TreeMap[Expr]): UnionTypeExpr = thisOr {
    copy(types = types.map(f))
  }

  override def updateMeta(
      updater: Option[ExprMeta] => Option[ExprMeta]
  ): UnionTypeExpr = copy(meta = updater(meta))

  override def toDoc(using PrettierOptions): Doc = group(
    Doc.wrapperlist(Doc.empty, Doc.empty, Doc.text(" | "))(types.map(_.toDoc))
  )
}
