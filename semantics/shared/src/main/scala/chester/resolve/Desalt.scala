package chester.resolve

import cats.data.*
import cats.implicits.*
import chester.error.*
import chester.syntax.{Const, ModuleRef}
import chester.syntax.concrete.*
import chester.utils.*
import chester.i18n.*

import scala.util.boundary
import scala.annotation.tailrec

case class DesugarInfo()

private object DesaltCaseClauseMatch {
  def unapply(
      x: Expr
  )(using reporter: Reporter[TyckProblem]): Option[DesaltCaseClause] = x match {
    case OpSeq(
          Vector(
            Identifier(Const.Case, _),
            pattern,
            Identifier(Const.Arrow2, _),
            returning
          ),
          meta
        ) =>
      Some(DesaltCaseClause(pattern, returning, meta))
    case OpSeq(Vector(Identifier(Const.Case, _), _*), _) =>
      val error = ExpectCase(x)
      reporter.report(error)
      None
    case _ => None
  }
}

private object MatchDeclarationTelescope {
  private def handleTerms(terms: Vector[Expr], x: Expr, implicitly: Boolean)(using reporter: Reporter[TyckProblem]): Option[DefTelescope] = {
    // Parameters enclosed in parentheses
    val argsResult = terms.map {
      case id: Identifier =>
        Some(Arg(name = id, meta = id.meta))
      case OpSeq(Vector(id: Identifier, Identifier(Const.`:`, _), ty), _) =>
        Some(Arg(name = id, ty = Some(ty), meta = id.meta))
      case _ =>
        reporter.report(ExpectParameterList(x))
        None
    }

    Option.unless(argsResult.contains(None))(DefTelescope(argsResult.flatten, implicitly = implicitly, meta = x.meta))
  }
  @tailrec
  def unapply(
      x: Expr
  )(using reporter: Reporter[TyckProblem]): Option[DefTelescope] = x match {
    case id: Identifier =>
      // Single parameter without type
      Some(DefTelescope(Vector(Arg(name = id, meta = id.meta)), meta = x.meta))
    case opseq @ OpSeq(terms, meta) if terms.nonEmpty => unapply(Tuple(Vector(opseq), meta))
    case t @ Tuple(terms, _)                          => handleTerms(terms, t, false)
    case t @ ListExpr(terms, _)                       => handleTerms(terms, t, true)
    case _ =>
      reporter.report(ExpectParameterList(x))
      None
  }
}

def opSeq(xs: Seq[Expr])(using Reporter[TyckProblem]): Expr =
  SimpleDesalt.desugar(OpSeq(xs.toVector, xs.head.meta))

private object DesaltSimpleFunction {
  private def predicate(x: Expr): Boolean = x match {
    case Identifier(Const.Arrow2, _) => true
    case _                           => false
  }

  def unapply(x: Expr)(using reporter: Reporter[TyckProblem]): Option[Expr] =
    x match {
      case OpSeq(xs, meta) if xs.exists(predicate) =>
        val index = xs.indexWhere(predicate)
        assert(index >= 0)
        val before = xs.take(index)
        val after = xs.drop(index + 1)

        val paramsExpr = before match {
          case x @ Vector(Tuple(_, _)) => x
          case _                       => before
        }

        paramsExpr.traverse(MatchDeclarationTelescope.unapply) match {
          case Some(telescopes) =>
            val body = opSeq(after)
            Some(
              FunctionExpr(
                telescope = telescopes,
                body = body,
                meta = meta
              )
            )
          case None =>
            val error = ExpectLambda(x)
            reporter.report(error)
            Some(DesaltFailed(x, error, meta))
        }
      case _ => None
    }
}

private object ObjectDesalt {
  def desugarQualifiedName(qname: QualifiedName): Vector[String] = qname match {
    case Identifier(name, _) => Vector(name)
    case DotCall(expr: QualifiedName, field: Identifier, _, _) =>
      desugarQualifiedName(expr) :+ field.name
    case _ =>
      throw new IllegalArgumentException("Invalid QualifiedName structure")
  }

  private def insertNested(
      fields: Vector[(Vector[String], Expr)],
      base: ObjectExpr
  ): ObjectExpr =
    fields.foldLeft(base) {
      case (acc, (Vector(k), v)) =>
        acc.copy(clauses = acc.clauses :+ ObjectExprClause(Identifier(k, v.meta), v))
      case (acc, (k +: ks, v)) =>
        val nestedObj = acc.clauses
          .collectFirst {
            case ObjectExprClause(id: Identifier, obj: ObjectExpr, _) if id.name == k =>
              obj
          }
          .getOrElse(ObjectExpr(Vector.empty, v.meta))
        val updatedNested = insertNested(Vector((ks, v)), nestedObj)
        val updatedClauses = acc.clauses.filterNot {
          case ObjectExprClause(id: Identifier, _, _) => id.name == k
          case _                                      => false
        } :+ ObjectExprClause(Identifier(k, v.meta), updatedNested)
        acc.copy(clauses = updatedClauses)
      case (acc, _) => acc
    }

  def desugarObjectExpr(expr: ObjectExpr): ObjectExpr = {
    val (desugaredFields, otherClauses) = expr.clauses.foldLeft(
      (Vector.empty[(Vector[String], Expr)], Vector.empty[ObjectClause])
    ) {
      case ((fields, others), ObjectExprClause(qname, value, _)) =>
        (fields :+ (desugarQualifiedName(qname), value), others)
      case ((fields, others), clause) =>
        (fields, others :+ clause)
    }

    val nestedObject = insertNested(desugaredFields, ObjectExpr(Vector.empty, meta = expr.meta))

    val updatedClauses = (nestedObject.clauses ++ otherClauses).map {
      case ObjectExprClause(key: Identifier, value, _) =>
        ObjectExprClauseOnValue(SymbolLiteral(key.name, key.meta), value)
      case other: ObjectExprClauseOnValue => other
      case _                              => unreachable()
    }

    expr.copy(clauses = updatedClauses)
  }
}

case object PatternDesalt {
  def desugar(
      x: Expr
  )(using Reporter[TyckProblem]): Option[DesaltPattern] = PartialFunction.condOpt(x) { case id @ Identifier(_, meta) =>
    PatternBind(id, meta) // TODO: more
  }
}

case object StmtDesalt {
  def desugar(x: Expr)(using Reporter[TyckProblem]): Expr = x match {
    case StmtDesalt(x) => x
    case _             => x
  }

  def defined(
      xs: Vector[Expr]
  )(using Reporter[TyckProblem]): Option[Defined] =
    if (xs.isEmpty) None
    else if (xs.length == 1) xs.head match {
      // TODO: support multiple telescopes
      case FunctionCall(f: Identifier, MatchDeclarationTelescope(t), _) =>
        Some(DefinedFunction(f, Vector(t).assumeNonEmpty))
      case a => PatternDesalt.desugar(a).map(DefinedPattern.apply)
    }
    else
      xs.head match {
        case identifier: Identifier =>
          xs.tail.traverse(MatchDeclarationTelescope.unapply).map { telescopes =>
            DefinedFunction(
              identifier,
              NonEmptyVector.fromVectorUnsafe(telescopes)
            )
          }
        case _ => None
      }

  private def letdef(
      decorations: Vector[Expr],
      kw: Identifier,
      xs: Vector[Expr],
      cause: Expr
  )(using reporter: Reporter[TyckProblem]): Stmt =
    boundary {
      val typeIdx = xs.indexWhere {
        case Identifier(Const.`:`, _) => true; case _ => false
      }
      val valueIdx = xs.indexWhere {
        case Identifier(Const.`=`, _) => true; case _ => false
      }
      val kind = kw.name match {
        case Const.Let => LetDefType.Let
        case Const.Def => LetDefType.Def
        case name      => unreachable(t"Unknown keyword $name")
      }

      val (onExprs, typeExprs, valueExprs) = (typeIdx, valueIdx) match {
        case (-1, -1)   => (xs, Vector.empty[Expr], Vector.empty[Expr])
        case (tIdx, -1) => (xs.take(tIdx), xs.drop(tIdx + 1), Vector.empty[Expr])
        case (-1, vIdx) => (xs.take(vIdx), Vector.empty[Expr], xs.drop(vIdx + 1))
        case (tIdx, vIdx) if tIdx < vIdx =>
          (xs.take(tIdx), xs.slice(tIdx + 1, vIdx), xs.drop(vIdx + 1))
        case _ =>
          val error = ExpectLetDef(cause)
          reporter.report(error)
          boundary.break(DesaltFailed(cause, error, cause.meta))
      }

      val on = defined(onExprs).getOrElse {
        val error = ExpectLetDef(cause)
        reporter.report(error)
        boundary.break(DesaltFailed(cause, error, cause.meta))
      }

      val ty = Option.when(typeExprs.nonEmpty)(opSeq(typeExprs))
      val body = Option.when(valueExprs.nonEmpty)(opSeq(valueExprs))
      unrollFunction(
        LetDefStmt(
          kind,
          on,
          ty = ty,
          body = body,
          decorations = decorations,
          meta = cause.meta
        )
      )
    }

  private def unrollFunction(stmt: LetDefStmt): LetDefStmt =
    stmt.defined match {
      case DefinedFunction(id, telescopes) =>
        require(stmt.decorations.isEmpty, "not supported yet")
        require(stmt.body.nonEmpty, "not supported yet")
        val expr = FunctionExpr(
          telescope = telescopes.toVector,
          resultTy = stmt.ty,
          effect = stmt.effect,
          body = stmt.body.get,
          meta = stmt.meta
        )
        stmt.copy(
          defined = DefinedPattern(PatternBind(id, id.meta)),
          body = Some(expr),
          ty = None,
          effect = None
        )
      case _ => stmt
    }

  def unapply(x: Expr)(using Reporter[TyckProblem]): Option[Stmt] =
    x match {
      case opseq @ OpSeq(seq, _) =>
        seq.indexWhere {
          case Identifier(id, _) if Const.kw1.contains(id) => true
          case _                                           => false
        } match {
          case -1 => None
          case kwIdx =>
            val kwId = seq(kwIdx).asInstanceOf[Identifier]
            val beforeKw = seq.take(kwIdx)
            val afterKw = seq.drop(kwIdx + 1)
            if (!beforeKw.forall(_.isInstanceOf[Identifier])) None
            else
              kwId.name match {
                case Const.Let | Const.Def =>
                  Some(letdef(beforeKw, kwId, afterKw, opseq))
                case other => unreachable(t"Unknown keyword $other")
              }
        }
      case _ => None
    }
}

case object SimpleDesalt {
  def desugar(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr =
    expr match {
      case OpSeq(xs, _) if xs.length == 1 => xs.head
      case _ @DesaltCaseClauseMatch(x)    => x

      case OpSeq(Vector(a, op: Identifier, b), meta) =>
        DotCall(a, op, Vector(DesaltCallingTelescope(Vector(CallingArg(expr = b, meta = b.meta)), meta = b.meta)), meta)

      case block @ Block(heads, tail, _)
          if heads.exists(_.isInstanceOf[DesaltCaseClause]) ||
            tail.exists(_.isInstanceOf[DesaltCaseClause]) =>
        val clauses = (heads ++ tail.toVector).collect { case clause: DesaltCaseClause =>
          clause
        }
        if (clauses.length != heads.length + tail.size) {
          val error = ExpectFullCaseBlock(block)
          reporter.report(error)
          DesaltFailed(block, error, block.meta)
        } else {
          DesaltMatching(clauses, block.meta)
        }
      case _ @Block(heads, tail, meta) =>
        Block(heads.map(StmtDesalt.desugar), tail.map(StmtDesalt.desugar), meta)
      case DesaltSimpleFunction(func) => func
      case obj: ObjectExpr            => ObjectDesalt.desugarObjectExpr(obj)
      case FunctionCall(function, telescopes, meta) =>
        val desugaredFunction = desugar(function)
        val desugaredTelescopes = telescopes match {
          case t: Tuple =>
            Vector(
              DesaltCallingTelescope(
                t.terms.map(term => CallingArg(expr = desugar(term), meta = term.meta)),
                meta = t.meta
              )
            )
          case other =>
            reporter.report(UnexpectedTelescope(other))
            Vector(
              DesaltCallingTelescope(
                Vector(CallingArg(expr = desugar(other), meta = other.meta)),
                meta = other.meta
              )
            )
        }
        desugaredFunction match {
          case DesaltFunctionCall(f, t, m) =>
            DesaltFunctionCall(f, t ++ desugaredTelescopes, m)
          case _ =>
            DesaltFunctionCall(
              desugaredFunction,
              NonEmptyVector.fromVectorUnsafe(desugaredTelescopes),
              meta
            )
        }
      case OpSeq(Vector(lhs, Identifier(Const.`:`, _), rhs), meta) =>
        val desugaredLhs = desugar(lhs)
        val desugaredRhs = desugar(rhs)
        TypeAnotationNoEffects(desugaredLhs, desugaredRhs, meta)
      case _ @OpSeq(Vector(Identifier(Const.Import, _), some), meta) =>
        Some(some) match {
          case Some(qualifiedName) =>
            val modulePath = ModuleRef(
              ObjectDesalt.desugarQualifiedName(
                qualifiedName.asInstanceOf[QualifiedName]
              )
            )
            ImportStmt(modulePath, meta)
        }
      case _ @OpSeq(Vector(Identifier(Const.Module, _), some), meta) =>
        Some(some) match {
          case Some(qualifiedName) =>
            val modulePath = ModuleRef(
              ObjectDesalt.desugarQualifiedName(
                qualifiedName.asInstanceOf[QualifiedName]
              )
            )
            ModuleStmt(modulePath, meta)
        }
      case expr @ OpSeq(Vector(Identifier(Const.Record, _), nameExpr, rest @ _*), meta) =>
        // Parse the record name and parameters if any
        val result = boundary {
          val (name, parameterExprs) = nameExpr match {
            case id: Identifier =>
              (id, Vector.empty[Expr])
            case FunctionCall(id: Identifier, telescope, _) =>
              (id, Vector(telescope))
            case _ =>
              val error = ExpectRecordName(nameExpr)
              reporter.report(error)
              boundary.break[Expr](DesaltFailed(expr, error, meta))
          }

          // Process the rest of the tokens
          var tokens = rest.toList

          // Use the common method to parse the optional ExtendsClause
          val (extendsClause, remainingTokens) = parseExtendsClause(tokens, meta)
          tokens = remainingTokens

          // Parse fields and body
          val (fieldExprs0, bodyExprs) = tokens.span {
            case Tuple(_, _) => true
            case _           => false
          }
          val fieldExprs = parameterExprs ++ fieldExprs0

          // Desugar fields into Field instances
          val desugaredFields = fieldExprs.flatMap {
            case Tuple(terms, _) =>
              terms.map {
                case OpSeq(Vector(id: Identifier, Identifier(Const.`:`, _), ty), _) =>
                  Some(RecordField(name = id, ty = Some(ty), meta = id.meta))
                case id: Identifier =>
                  Some(RecordField(name = id, meta = id.meta))
                case other =>
                  reporter.report(ExpectFieldDeclaration(other))
                  None
              }
            case other =>
              reporter.report(ExpectFieldDeclaration(other))
              None
          }.flatten

          // Desugar body if present
          val desugaredBody = if (bodyExprs.nonEmpty) {
            val bodyExpr = opSeq(bodyExprs)
            Some(desugar(bodyExpr) match {
              case b: Block => b
              case other    => Block(Vector(other), None, meta = other.meta)
            })
          } else None

          RecordStmt(
            name = name,
            extendsClause = extendsClause,
            fields = desugaredFields,
            body = desugaredBody,
            meta = meta
          )
        }
        result

      // Handling 'trait' keyword
      case expr @ OpSeq(Vector(Identifier(Const.Trait, _), nameExpr, rest @ _*), meta) =>
        // Parse the trait name and parameters if any
        val result = boundary {
          val (name, parameterExprs) = nameExpr match {
            case id: Identifier =>
              (id, Vector.empty[Expr])
            case FunctionCall(id: Identifier, telescope, _) =>
              (id, Vector(telescope))
            case _ =>
              val error = ExpectTraitName(nameExpr)
              reporter.report(error)
              boundary.break[Expr](DesaltFailed(expr, error, meta))
          }

          // Process the rest of the tokens
          var tokens = rest.toList

          // Use the common method to parse the optional ExtendsClause
          val (extendsClause, remainingTokens) = parseExtendsClause(tokens, meta)
          tokens = remainingTokens

          // Parse body if present
          val bodyExpr = if (tokens.nonEmpty) {
            val body = opSeq(tokens)
            Some(desugar(body) match {
              case b: Block => b
              case other    => Block(Vector(other), None, meta = other.meta)
            })
          } else None

          TraitStmt(
            name = name,
            extendsClause = extendsClause,
            body = bodyExpr,
            meta = meta
          )
        }
        result

      case expr @ OpSeq(Vector(Identifier(Const.Object, _), nameExpr, rest @ _*), meta) =>
        // Parse the object name
        val result = boundary {
          val name = nameExpr match {
            case id: Identifier => id
            case _ =>
              val error = ExpectObjectName(nameExpr)
              reporter.report(error)
              boundary.break[Expr](DesaltFailed(expr, error, meta))
          }

          // Process the rest of the tokens
          val tokens = rest.toList

          // Parse the optional ExtendsClause
          val (extendsClause, remainingTokens) = parseExtendsClause(tokens, meta)

          // Parse body if present
          val bodyExpr = if (remainingTokens.nonEmpty) {
            val body = opSeq(remainingTokens)
            Some(desugar(body) match {
              case b: Block => b
              case other    => Block(Vector(other), None, meta = other.meta)
            })
          } else None

          ObjectStmt(
            name = name,
            extendsClause = extendsClause,
            body = bodyExpr,
            meta = meta
          )
        }
        result

      // Handling 'interface' keyword
      case expr @ OpSeq(Vector(Identifier(Const.Interface, _), nameExpr, rest @ _*), meta) =>
        // Parse the interface name and parameters if any
        val result = boundary {
          val (name, parameterExprs) = nameExpr match {
            case id: Identifier =>
              (id, Vector.empty[Expr])
            case FunctionCall(id: Identifier, telescope, _) =>
              (id, Vector(telescope))
            case _ =>
              val error = ExpectInterfaceName(nameExpr)
              reporter.report(error)
              boundary.break[Expr](DesaltFailed(expr, error, meta))
          }

          // Process the rest of the tokens
          var tokens = rest.toList

          // Use the common method to parse the optional ExtendsClause
          val (extendsClause, remainingTokens) = parseExtendsClause(tokens, meta)
          tokens = remainingTokens

          // Parse body if present
          val bodyExpr = if (tokens.nonEmpty) {
            val body = opSeq(tokens)
            Some(desugar(body) match {
              case b: Block => b
              case other    => Block(Vector(other), None, meta = other.meta)
            })
          } else None

          InterfaceStmt(
            name = name,
            extendsClause = extendsClause,
            body = bodyExpr,
            meta = meta
          )
        }
        result

      case default => default
    }

  @tailrec
  private def unwrap(e: Expr)(using Reporter[TyckProblem]): Expr =
    e match {
      case Block(Vector(), Some(tail), _) => unwrap(desugar(tail))
      case Tuple(Vector(term), _)         => unwrap(desugar(term))
      case _                              => e
    }
  def desugarTele(expr: Expr)(using Reporter[TyckProblem]): Expr =
    MatchDeclarationTelescope.unapply(expr)
  def desugarUnwrap(expr: Expr)(using Reporter[TyckProblem]): Expr =
    unwrap(desugar(expr))

  // Helper method to parse super types separated by 'with'
  private def parseSuperTypes(tokens: List[Expr]): (List[Expr], List[Expr]) = {
    @tailrec
    def loop(accum: List[Expr], remaining: List[Expr]): (List[Expr], List[Expr]) =
      remaining match {
        case Identifier(Const.`with`, _) :: next :: tail =>
          loop(next :: accum, tail)
        case _ =>
          (accum.reverse, remaining)
      }
    tokens match {
      case firstType :: tail => loop(List(firstType), tail)
      case Nil               => (Nil, Nil)
    }
  }

  private def parseExtendsClause(tokens: List[Expr], meta: Option[ExprMeta]): (Option[ExtendsClause], List[Expr]) =
    tokens match {
      case Identifier(Const.`<:`, _) :: rest =>
        val (superTypes, remainingTokens) = parseSuperTypes(rest)
        (Some(ExtendsClause(superTypes.toVector, meta)), remainingTokens)
      case _ =>
        (None, tokens)
    }
}

case object OpSeqDesalt {
  def desugar(expr: Expr)(using reporter: Reporter[TyckProblem]): Expr = expr match {
    case opseq @ OpSeq(seq, meta) =>
      // Look for pipe operators in the sequence
      val pipeIndices = seq.zipWithIndex.collect { case (Identifier("|", _), idx) =>
        idx
      }

      if (pipeIndices.isEmpty) {
        // No pipe operators found, return the original expression
        opseq
      } else {
        // Found pipe operators, construct a union type
        // We need to ensure the sequence is properly formed like: A | B | C
        // Where the pipe operators should be at odd indices (1, 3, 5, ...)

        // Check if the first pipe operator is at an odd index (meaning we have a type before it)
        val firstPipeIdx = pipeIndices.head
        if (firstPipeIdx % 2 == 0) {
          // Invalid format, pipe operator should be preceded by a type
          reporter.report(NotImplemented(opseq))
          return opseq
        }

        // Check if all pipe operators are at odd indices
        val validPipePositions = pipeIndices.forall(_ % 2 == 1)
        if (!validPipePositions) {
          // Invalid format, pipe operators should alternate with types
          reporter.report(NotImplemented(opseq))
          return opseq
        }

        // Extract the types (which should be at even indices: 0, 2, 4, ...)
        val typeIndices = seq.indices.filter(_ % 2 == 0)
        if (typeIndices.isEmpty) {
          reporter.report(NotImplemented(opseq))
          return opseq
        }

        // Process each individual type expression through SimpleDesalt
        val types = typeIndices.map(idx => SimpleDesalt.desugar(seq(idx))).toVector

        // Create a UnionTypeExpr with the extracted types
        UnionTypeExpr(types, meta)
      }
    case _ => expr
  }
}
