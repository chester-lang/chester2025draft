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
  // Sealed trait for declaration information, for forwarding references
  sealed trait DeclarationInfo {
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

  def elabBlock(expr: Block, ty0: CellIdOr[Term], effects: CIdOf[EffectsCell])(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): BlockTerm
}

trait ProvideElaboraterBlock extends ElaboraterBlock {
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

    val stmts: Vector[StmtTerm] = heads.flatMapOrdered {
      case expr: LetDefStmt if expr.kind == LetDefType.Def =>
        val (stmtTerms, newCtx) = processDefLetDefStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      case expr: RecordStmt =>
        val (stmtTerms, newCtx) = processRecordStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      // Process trait statements
      case expr: TraitStmt =>
        val (stmtTerms, newCtx) = processTraitStmt(expr, ctx, declarationsMap, effects)
        ctx = newCtx
        stmtTerms

      // Process interface statements
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

      // Process object statements
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

  def collectDeclarations(
      heads: Seq[Expr],
      meta: Option[ExprMeta]
  )(using
      localCtx: Context,
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[DeclarationInfo], Seq[Name], Context) = {

    // Collect all declarations in a single pass
    val declarations = heads.collect {
      // Collect 'def' declarations
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

      // Collect 'record' declarations
      case expr: RecordStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[RecordStmtTerm]
        RecordDeclaration(expr, id, name)

      // Collect 'trait' declarations
      case expr: TraitStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[TraitStmtTerm]
        TraitDeclaration(expr, id, name)

      // Collect 'interface' declarations
      case expr: InterfaceStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[InterfaceStmtTerm]
        InterfaceDeclaration(expr, id, name)

      // Collect 'object' declarations
      case expr: ObjectStmt =>
        val name = expr.name.name
        val id = Uniqid.generate[ObjectStmtTerm]
        ObjectDeclaration(expr, id, name)
    }

    val names = declarations.map(_.name)

    // Collect context items from 'def' declarations
    val defContextItems = declarations.collect { case defDecl: DefDeclaration =>
      defDecl.item
    }
    val initialCtx = localCtx.add(defContextItems)

    // Return all declarations, names, and the initial context
    (declarations, names, initialCtx)
  }

  def checkForDuplicateNames(names: Seq[Name], expr: Expr)(using ck: Tyck): Unit = {
    if (names.hasDuplication) {
      val problem = DuplicateDefinition(expr)
      ck.reporter.apply(problem)
    }
  }
  def processDefLetDefStmt(
      expr: LetDefStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
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
    val name = recordInfo.name

    // Extract the fields from the record
    val fields = expr.fields

    // Extract the symbol from the extendsClause, if any. TODO: this is a stub only
    val _ = expr.extendsClause.map { case clause @ ExtendsClause(superType, _) =>
      superType.head match {
        case Identifier(superName, _) => superName
        case _                        =>
          // For now, only handle simple identifiers; report a warning or error if needed
          ck.reporter.apply(UnsupportedExtendsType(clause))
          // Return None since we cannot handle complex types yet
          return (Seq.empty, ctx)
      }
    }

    // Elaborate the fields without combining them with any super class fields
    val elaboratedFields = fields.map { field =>
      val fieldType = field.ty match {
        case Some(tyExpr) => checkType(tyExpr)
        case None         => newTypeTerm
      }
      // Create a FieldTerm representing the field in the record
      FieldTerm(field.name.name, fieldType, convertMeta(expr.meta))
    }

    // Elaborate the optional body (if any)
    val elaboratedBody = expr.body.map { body =>
      elabBlock(body, newTypeTerm, effects)(using ctx, parameter, ck, state)
    }

    // Construct the RecordStmtTerm that includes the fields and extendsClause
    val recordStmtTerm = RecordStmtTerm(
      name = name,
      uniqId = recordInfo.uniqId,
      fields = elaboratedFields,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
      // We can store the extendsSymbol here if needed in the future
      // For now, we add a TODO comment
      // TODO: Handle the extendsClause during type checking
    )

    // Update the context with the new record definition
    val newCtx = ctx
      .addTypeDefinition(recordStmtTerm)

    // Return the statement term and the updated context
    (Seq(recordStmtTerm), newCtx)
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
    implicit val localCtx: Context = ctx
    val traitInfo = declarationsMap(expr).asInstanceOf[TraitDeclaration]
    val name = traitInfo.name

    // TODO: Elaborate the extends clause properly
    val elaboratedExtendsClause = expr.extendsClause.map { clause =>
      checkType(clause)
    }

    // Elaborate the optional body (if any)
    val elaboratedBody = expr.body.map { body =>
      elabBlock(body, newTypeTerm, effects)(using ctx, parameter, ck, state)
    }

    // Create the TraitStmtTerm
    val traitStmtTerm = TraitStmtTerm(
      name = name,
      uniqId = traitInfo.uniqId,
      extendsClause = elaboratedExtendsClause,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
    )

    // Update the context with the new trait definition
    val newCtx = ctx.addTypeDefinition(traitStmtTerm)

    // Return the statement term and the updated context
    (Seq(traitStmtTerm), newCtx)
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
    val elaboratedExtendsClause = expr.extendsClause.map { clause =>
      checkType(clause)
    }

    // Elaborate the optional body (if any)
    val elaboratedBody = expr.body.map { body =>
      elabBlock(body, newTypeTerm, effects)(using ctx, parameter, ck, state)
    }

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

  def processObjectStmt(
      expr: ObjectStmt,
      ctx: Context,
      declarationsMap: Map[Expr, DeclarationInfo],
      effects: CIdOf[EffectsCell]
  )(using
      parameter: SemanticCollector,
      ck: Tyck,
      state: StateAbility[Tyck]
  ): (Seq[StmtTerm], Context) = {
    implicit val localCtx: Context = ctx
    val objectInfo = declarationsMap(expr).asInstanceOf[ObjectDeclaration]
    val name = objectInfo.name

    // Elaborate the extends clause if present
    val elaboratedExtendsClause = expr.extendsClause.map { clause =>
      checkType(clause)
    }

    // Elaborate the body if present
    val elaboratedBody = expr.body.map { body =>
      elabBlock(body, newTypeTerm, effects)(using ctx, parameter, ck, state)
    }

    // Create the ObjectStmtTerm
    val objectStmtTerm = ObjectStmtTerm(
      name = name,
      uniqId = objectInfo.uniqId,
      extendsClause = elaboratedExtendsClause,
      body = elaboratedBody,
      meta = convertMeta(expr.meta)
    )

    val newCtx = ctx.addTypeDefinition(objectStmtTerm)

    // Return the statement term and the updated context
    (Seq(objectStmtTerm), newCtx)
  }
}
