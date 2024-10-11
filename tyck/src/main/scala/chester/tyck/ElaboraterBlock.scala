package chester.tyck

import cats.implicits.*
import chester.tyck.*
import chester.utils.*
import chester.syntax.*
import scala.language.implicitConversions
import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector
import chester.uniqid.*

trait ElaboraterBlock extends Elaborater {
  case class DefInfo(
      expr: LetDefStmt,
      id: UniqIdOf[LocalV],
      tyAndVal: TyAndVal,
      item: ContextItem
  )
  def elabBlock(expr: Block, ty0: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: LocalCtx,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): BlockTerm
}

trait ProvideElaboraterBlock extends ElaboraterBlock {

  def elabBlock(expr: Block, ty0: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: LocalCtx,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): BlockTerm = {
    val ty = toId(readMetaVar(toTerm(ty0)))
    val Block(heads0, tail, meta) = expr
    val heads = heads0.map(resolve)

    val (defs, names, initialCtx) = collectDefs(heads, meta)
    checkForDuplicateNames(names, expr)

    var ctx = initialCtx
    val defsMap = defs.map(info => (info.expr, info)).toMap

    val stmts: Seq[StmtTerm] = heads.flatMapOrdered {
      case expr: LetDefStmt if expr.kind == LetDefType.Def =>
        val (stmtTerms, newCtx) = processDefLetDefStmt(expr, ctx, defsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr: LetDefStmt if expr.kind == LetDefType.Let =>
        val (stmtTerms, newCtx) = processLetLetDefStmt(expr, ctx, effects, meta)
        ctx = newCtx
        stmtTerms

      case importStmt: ImportStmt =>
        ck.reporter.apply(NotImplemented(importStmt))
        Vector()

      case expr =>
        implicit val localCtx: LocalCtx = ctx
        val ty = newType
        Vector(ExprStmtTerm(elab(expr, ty, effects), Meta(ty)))
    }

    {

      implicit val localCtx: LocalCtx = ctx
      val tailExpr = tail.getOrElse(UnitExpr(meta))
      val wellTyped = elab(tailExpr, ty, effects)
      BlockTerm(stmts, wellTyped)
    }
  }

  def collectDefs(
      heads: Seq[Expr],
      meta: Option[ExprMeta]
  )(using
      localCtx: LocalCtx,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[DefInfo], Seq[Name], LocalCtx) = {
    val defs = heads.collect {
      case expr: LetDefStmt if expr.kind == LetDefType.Def =>
        val name = expr.defined match {
          // TODO: support other defined patterns
          case DefinedPattern(PatternBind(name, _)) => name.name
        }
        val tyandval = TyAndVal.create()
        val id = UniqId.generate[LocalV]
        val localv = newLocalv(name, tyandval.ty, id, meta)
        val r = parameter.newSymbol(localv, id, expr)
        DefInfo(
          expr,
          id,
          tyandval,
          ContextItem(name, id, localv, tyandval.ty, Some(r))
        )
    }
    val names = defs.map(_.item.name)
    val initialCtx = localCtx.add(defs.map(_.item))
    (defs, names, initialCtx)
  }

  def checkForDuplicateNames(names: Seq[Name], expr: Expr)(using ck: Tyck): Unit = {
    if (names.hasDuplication) {
      val problem = DuplicateDefinition(expr)
      ck.reporter.apply(problem)
    }
  }

  def processDefLetDefStmt(
      expr: LetDefStmt,
      ctx: LocalCtx,
      defsMap: Map[LetDefStmt, DefInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], LocalCtx) = {
    implicit val localCtx: LocalCtx = ctx
    val defInfo = defsMap(expr)
    val ty = expr.ty match {
      case Some(tyExpr) =>
        val t = checkTypeId(tyExpr)
        merge(t, defInfo.tyAndVal.tyId)
        t
      case None => defInfo.tyAndVal.ty
    }
    val wellTyped = elabId(expr.body.get, ty, effects)
    merge(defInfo.tyAndVal.valueId, wellTyped)
    val newCtx = ctx.knownAdd(defInfo.id, TyAndVal(ty, wellTyped))
    (
      Vector(DefStmtTerm(defInfo.item.name, Meta(wellTyped), toTerm(ty))),
      newCtx
    )
  }

  def processLetLetDefStmt(
      expr: LetDefStmt,
      ctx: LocalCtx,
      effects: CIdOf[EffectsCell],
      meta: Option[ExprMeta]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], LocalCtx) = {
    implicit val localCtx: LocalCtx = ctx
    val name = expr.defined match {
      // TODO: support other defined patterns
      case DefinedPattern(PatternBind(name, _)) => name.name
    }
    val id = UniqId.generate[LocalV]
    val ty = expr.ty match {
      case Some(tyExpr) => checkType(tyExpr)
      case None         => newTypeTerm
    }
    val localv = newLocalv(name, ty, id, meta)
    val r = parameter.newSymbol(localv, id, expr)
    val wellTyped = elab(expr.body.get, ty, effects)
    val newCtx = ctx
      .add(ContextItem(name, id, localv, ty, Some(r)))
      .knownAdd(id, TyAndVal(ty, wellTyped))
    (
      Vector(LetStmtTerm(name, wellTyped, ty)),
      newCtx
    )
  }

}
