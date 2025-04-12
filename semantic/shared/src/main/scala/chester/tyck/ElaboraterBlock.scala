package chester.tyck

import chester.utils.*
import chester.syntax.*
import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.SemanticCollector
import chester.uniqid.*

import scala.language.implicitConversions

object ElaboraterBlock {
  // Sealed trait for declaration information, for forwarding references
  sealed trait DeclarationInfo extends Product with Serializable {
    def expr: Expr
    def name: Name
  }

  // Case class for 'def' declarations
  case class DefDeclaration(
      expr: LetDefStmt,
      localv: LocalV,
      tyAndVal: TyAndVal,
      item: ContextItem
  ) extends DeclarationInfo {
    def name: Name = item.name
  }

  // Case class for 'record' declarations
  case class RecordDeclaration(
      expr: RecordStmt,
      uniqId: UniqidOf[RecordStmtTerm],
      name: Name
  ) extends DeclarationInfo

  // New declarations for trait and interface
  case class TraitDeclaration(
      expr: TraitStmt,
      uniqId: UniqidOf[TraitStmtTerm],
      name: Name
  ) extends DeclarationInfo

  case class InterfaceDeclaration(
      expr: InterfaceStmt,
      uniqId: UniqidOf[InterfaceStmtTerm],
      name: Name
  ) extends DeclarationInfo

  // Add case class for 'object' declarations
  case class ObjectDeclaration(
      expr: ObjectStmt,
      uniqId: UniqidOf[ObjectStmtTerm],
      name: Name
  ) extends DeclarationInfo
}

trait ElaboraterBlock { this: ElaboraterBase & ElaboraterCommon =>

  def elabBlock(expr: Block, ty0: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): BlockTerm
}

trait ProvideElaboraterBlock extends ElaboraterBlock { this: Elaborater & ElaboraterBase & ElaboraterCommon =>
  import ElaboraterBlock.*

  def elabBlock(expr: Block, ty0: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): BlockTerm = {
    val ty = toId(readMetaVar(toTerm(ty0)))
    val Block(heads0, tail, meta) = expr
    val heads = heads0.map(resolve)

    val (declarations, names, initialCtx) = collectDeclarations(heads, meta)
    checkForDuplicateNames(names, expr)

    var ctx = initialCtx
    val declarationsMap = declarations.map(info => (info.expr, info)).toMap

    // Determine if we're in a trait body
    val isInTraitBody = localCtx.currentProcessingType == "trait"

    val stmts: Vector[StmtTerm] = heads.flatMapOrdered {
      case expr: LetDefStmt if expr.kind == LetDefType.Def =>
        if (isInTraitBody) {
          // Process trait field declarations as DefStmtTerm
          val name = expr.defined match {
            case DefinedPattern(PatternBind(name, _)) => name.name
            case _                                    => ???
          }
          val ty = expr.ty match {
            case Some(tyExpr) => checkType(tyExpr)
            case None         => newTypeTerm
          }
          val id = Uniqid.generate[LocalV]
          val localv = newLocalv(name, ty, id, expr.meta)
          Vector(DefStmtTerm(localv, Meta(newType), ty, convertMeta(expr.meta)))
        } else {
          val (stmtTerms, newCtx) = processDefLetDefStmt(expr, ctx, declarationsMap, effects)
          ctx = newCtx
          stmtTerms
        }

      case expr: RecordStmt =>
        val (stmtTerms, newCtx) = processRecordStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr: TraitStmt =>
        val (stmtTerms, newCtx) = processTraitStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr: InterfaceStmt =>
        val (stmtTerms, newCtx) = processInterfaceStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr: LetDefStmt if expr.kind == LetDefType.Let =>
        val (stmtTerms, newCtx) = processLetLetDefStmt(expr, ctx, effects, meta)
        ctx = newCtx
        stmtTerms

      case importStmt: ImportStmt =>
        ck.reporter.apply(NotImplemented(importStmt))
        Vector.empty

      case expr: ObjectStmt =>
        val (stmtTerms, newCtx) = processObjectStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr =>
        implicit val localCtx: Context = ctx
        val ty = newType
        Vector(ExprStmtTerm(elab(expr, ty, effects), Meta(ty), convertMeta(expr.meta)))
    }

    // Block is needed for implicit locals, don't remove
    {
      implicit val localCtx: Context = ctx
      val tailExpr = tail.getOrElse(UnitExpr(meta))
      val wellTyped = elab(tailExpr, ty, effects)
      BlockTerm(stmts, wellTyped, meta = convertMeta(expr.meta))
    }
  }

  private def collectDeclarations(
      heads: Seq[Expr],
      meta: Option[ExprMeta]
  )(using
      localCtx: Context,
      parameter: SemanticCollector,
      _ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[DeclarationInfo], Seq[Name], Context) = {
    // Collect all declarations in a single pass
    val declarations = heads.collect {
      case expr: LetDefStmt if expr.kind == LetDefType.Def =>
        val name = expr.defined match {
          // TODO: support other defined patterns
          case DefinedPattern(PatternBind(name, _)) => name.name
          case _                                    => ???
        }
        val tyAndVal = TyAndVal.create()
        val id = Uniqid.generate[LocalV]
        val localv = newLocalv(name, tyAndVal.ty, id, meta)
        val r = parameter.newSymbol(localv, id, expr, localCtx)
        DefDeclaration(
          expr,
          localv,
          tyAndVal,
          ContextItem(name, id, localv, tyAndVal.ty, Some(r))
        )

      case expr: RecordStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[RecordStmtTerm]
        RecordDeclaration(expr, id, name)

      case expr: TraitStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[TraitStmtTerm]
        TraitDeclaration(expr, id, name)

      case expr: InterfaceStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[InterfaceStmtTerm]
        InterfaceDeclaration(expr, id, name)

      case expr: ObjectStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[ObjectStmtTerm]
        ObjectDeclaration(expr, id, name)
    }

    val names = declarations.map(_.name)
    val defContextItems = declarations.collect { case defDecl: DefDeclaration =>
      defDecl.item
    }
    val initialCtx = localCtx.add(defContextItems)

    (declarations, names, initialCtx)
  }

  private def checkForDuplicateNames(names: Seq[Name], expr: Expr)(using ck: Tyck): Unit =
    if (names.hasDuplication) {
      val problem = DuplicateDefinition(expr)
      ck.reporter.apply(problem)
    }

  private def processDefLetDefStmt(
      expr: LetDefStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val defInfo = declarationsMap(expr).asInstanceOf[DefDeclaration]
    val ty = expr.ty match {
      case Some(tyExpr) =>
        val t = checkTypeId(tyExpr)
        merge(t, defInfo.tyAndVal.tyId)
        t
      case None => defInfo.tyAndVal.ty
    }
    val wellTyped = elabId(expr.body.get, ty, effects)
    merge(defInfo.tyAndVal.valueId, wellTyped)
    val newCtx = ctx.knownAdd(defInfo.localv.uniqId, TyAndVal(toTerm(ty), toTerm(wellTyped)))
    (
      Vector(DefStmtTerm(defInfo.localv, Meta(wellTyped), toTerm(ty), convertMeta(expr.meta))),
      newCtx
    )
  }

  def processRecordStmt(
      expr: RecordStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val recordInfo = declarationsMap(expr).asInstanceOf[RecordDeclaration]

    // Extract and elaborate the extends clause
    val elaboratedExtendsClause = expr.extendsClause.map { case clause @ ExtendsClause(superTypes, _) =>
      superTypes.headOption
        .map {
          case Identifier(traitName, _) =>
            ctx.getTypeDefinition(traitName) match {
              case Some(traitDef: TraitStmtTerm) =>
                val traitType = TraitTypeTerm(traitDef, convertMeta(clause.meta))
                // Create a record type for this record
                val recordStmtTerm = RecordStmtTerm(
                  name = recordInfo.name,
                  uniqId = recordInfo.uniqId,
                  fields = expr.fields.map { field =>
                    val fieldType = field.ty.map(checkType).getOrElse(newTypeTerm)
                    FieldTerm(field.name.name, fieldType, convertMeta(expr.meta))
                  },
                  body = None,
                  extendsClause = Some(traitType),
                  meta = convertMeta(expr.meta)
                )
                // Check that the record implements the trait
                state.addPropagator(CheckTraitImplementation(recordStmtTerm, traitDef, expr))
                traitType
              case _ =>
                ck.reporter.apply(NotATrait(superTypes.head))
                ErrorTerm(NotATrait(superTypes.head), convertMeta(clause.meta))
            }
          case superTypeExpr =>
            ck.reporter.apply(UnsupportedExtendsType(superTypeExpr))
            ErrorTerm(UnsupportedExtendsType(superTypeExpr), convertMeta(clause.meta))
        }
        .getOrElse(ErrorTerm(UnsupportedExtendsType(clause), convertMeta(clause.meta)))
    }

    // Elaborate the fields and body
    val elaboratedFields = expr.fields.map { field =>
      val fieldType = field.ty.map(checkType).getOrElse(newTypeTerm)
      FieldTerm(field.name.name, fieldType, convertMeta(expr.meta))
    }

    val elaboratedBody = expr.body.map(body => elabBlock(body, newTypeTerm, effects))

    // Create the record statement term and update the context
    val recordStmtTerm = RecordStmtTerm(
      name = recordInfo.name,
      uniqId = recordInfo.uniqId,
      fields = elaboratedFields,
      body = elaboratedBody,
      extendsClause = elaboratedExtendsClause,
      meta = convertMeta(expr.meta)
    )

    (Seq(recordStmtTerm), ctx.addTypeDefinition(recordStmtTerm))
  }

  def processLetLetDefStmt(
      expr: LetDefStmt,
      ctx: Context,
      effects: CIdOf[EffectsCell],
      meta: Option[ExprMeta]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val name = expr.defined match {
      // TODO: support other defined patterns
      case DefinedPattern(PatternBind(name, _)) => name.name
      case _                                    => ???
    }
    val id = Uniqid.generate[LocalV]
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
      Vector(LetStmtTerm(localv, wellTyped, ty, convertMeta(expr.meta))),
      newCtx
    )
  }

  def processTraitStmt(
      expr: TraitStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    val traitInfo = declarationsMap(expr).asInstanceOf[TraitDeclaration]

    // Process extends clause if present
    val elaboratedExtendsClause = expr.extendsClause.map { case clause @ ExtendsClause(superTypes, _) =>
      superTypes.headOption
        .map {
          case Identifier(traitName, _) =>
            ctx.getTypeDefinition(traitName) match {
              case Some(traitDef: TraitStmtTerm) =>
                TraitTypeTerm(traitDef, convertMeta(clause.meta))
              case _ =>
                ck.reporter.apply(NotATrait(superTypes.head))
                ErrorTerm(NotATrait(superTypes.head), convertMeta(clause.meta))
            }
          case superTypeExpr =>
            ck.reporter.apply(UnsupportedExtendsType(superTypeExpr))
            ErrorTerm(UnsupportedExtendsType(superTypeExpr), convertMeta(clause.meta))
        }
        .getOrElse(ErrorTerm(UnsupportedExtendsType(clause), convertMeta(clause.meta)))
    }

    // Elaborate the body within a trait-specific context
    val elaboratedBody = expr.body.map(body => elabBlock(body, newTypeTerm, effects)(using ctx.withProcessingType("trait"), parameter, ck, state))

    // Create the TraitStmtTerm
    val traitStmtTerm = TraitStmtTerm(
      name = traitInfo.name,
      uniqId = traitInfo.uniqId,
      extendsClause = elaboratedExtendsClause,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
    )

    (Seq(traitStmtTerm), ctx.addTypeDefinition(traitStmtTerm))
  }

  def processInterfaceStmt(
      expr: InterfaceStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val interfaceInfo = declarationsMap(expr).asInstanceOf[InterfaceDeclaration]
    val name = interfaceInfo.name

    // TODO: Elaborate the extends clause properly
    val elaboratedExtendsClause = expr.extendsClause.map(checkType)

    // Elaborate the optional body (if any)
    val elaboratedBody = expr.body.map(body => elabBlock(body, newTypeTerm, effects)(using ctx, parameter, ck, state))

    // Create the InterfaceStmtTerm
    val interfaceStmtTerm = InterfaceStmtTerm(
      name = name,
      uniqId = interfaceInfo.uniqId,
      extendsClause = elaboratedExtendsClause,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
    )

    // Update the context with the new interface definition
    val newCtx = ctx.addTypeDefinition(interfaceStmtTerm)

    // Return the statement term and the updated context
    (Seq(interfaceStmtTerm), newCtx)
  }

  private def processObjectStmt(
      expr: ObjectStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      SemanticCollector,
      Tyck,
      StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val objectInfo = declarationsMap(expr).asInstanceOf[ObjectDeclaration]

    val elaboratedExtendsClause = expr.extendsClause.map(checkType)
    val elaboratedBody = expr.body.map(body => elabBlock(body, newTypeTerm, effects))

    val objectStmtTerm = ObjectStmtTerm(
      name = objectInfo.name,
      uniqId = objectInfo.uniqId,
      extendsClause = elaboratedExtendsClause,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
    )

    (Seq(objectStmtTerm), ctx.addTypeDefinition(objectStmtTerm))
  }
}
