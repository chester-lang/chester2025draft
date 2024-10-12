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
  def processRecordStmt(
      expr: RecordStmt,
      ctx: LocalCtx,
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], LocalCtx) = {
    implicit val localCtx: LocalCtx = ctx

    // Extract the record name and fields
    val name = expr.name.name
    val fields = expr.fields

    // Extract the symbol from the extendsClause, if any
    val extendsSymbolOpt = expr.extendsClause.map { case ExtendsClause(superType, _) =>
      superType match {
        case Identifier(superName, _) => superName
        case _                        =>
          // For now, only handle simple identifiers; report a warning or error if needed
          ck.reporter.apply(UnsupportedExtendsType(superType))
          // Return None since we cannot handle complex types yet
          return (Seq.empty, ctx)
      }
    }

    // Create a new type for the record
    val recordType = newType

    // Generate a unique ID for the record
    val recordId = UniqId.generate[LocalV]

    // Create a new local variable for the record
    val recordV = newLocalv(name, recordType, recordId, expr.meta)

    // Add the record to the semantic collector
    val r = parameter.newSymbol(recordV, recordId, expr, localCtx)

    // Create the RecordDefinition with the extendsSymbol
    val recordDef = RecordDefinition(name, Vector.empty, extendsSymbolOpt, recordId, recordType)

    // Update the context with the new record definition
    val newCtx = ctx
      .add(ContextItem(name, recordId, recordV, recordType, Some(r)))
      .addRecordDefinition(recordDef)

    // Elaborate the fields without combining them with any super class fields
    val elaboratedFields = fields.map { field =>
      val fieldType = field.ty match {
        case Some(tyExpr) => checkType(tyExpr)
        case None         => newTypeTerm
      }

      // Create a FieldTerm representing the field in the record
      FieldTerm(field.name.name, fieldType)
    }

    // Elaborate the optional body (if any)
    val elaboratedBody = expr.body.map { body =>
      elabBlock(body, newTypeTerm, effects)(using newCtx, parameter, ck, state)
    }

    // Construct the RecordStmtTerm that includes the fields and extendsClause
    val recordStmtTerm = RecordStmtTerm(
      name = name,
      fields = elaboratedFields,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
      // We can store the extendsSymbol here if needed in the future
      // For now, we add a TODO comment
      // TODO: Handle the extendsClause during type checking
    )

    // Return the statement term and the updated context
    (Seq(recordStmtTerm), newCtx)
  }
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

      case expr: RecordStmt =>
        val (stmtTerms, newCtx) = processRecordStmt(expr, ctx, effects)
        ctx = newCtx
        stmtTerms

      case importStmt: ImportStmt =>
        ck.reporter.apply(NotImplemented(importStmt))
        Vector.empty

      case expr =>
        implicit val localCtx: LocalCtx = ctx
        val ty = newType
        Vector(ExprStmtTerm(elab(expr, ty, effects), Meta(ty)))
    }

    // block is needed for implicit locals, don't remove
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
        val r = parameter.newSymbol(localv, id, expr, localCtx)
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
    val r = parameter.newSymbol(localv, id, expr, localCtx)
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
