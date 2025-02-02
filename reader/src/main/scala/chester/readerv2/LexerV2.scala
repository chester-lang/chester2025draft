package chester.readerv2

import chester.error.{Pos, SourcePos, RangeInFile}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.*
import spire.math.Rational
import scala.util.boundary
import scala.util.boundary.break

case class LexerState(
  tokens: TokenStream,
  current: Either[ParseError, Token],
  ignoreLocation: Boolean,
  sourceOffset: SourceOffset
)

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  private val state: LexerState = {
    val current = tokens.head
    LexerState(tokens.tail, current, ignoreLocation, sourceOffset)
  }

  private def createMeta(startPos: Pos, endPos: Pos): Option[ExprMeta] = {
    if (ignoreLocation) None
    else {
      val sourcePos = SourcePos(sourceOffset, RangeInFile(startPos, endPos))
      Some(ExprMeta(Some(sourcePos), None))
    }
  }

  private def advance(state: LexerState): LexerState = {
    val current = state.tokens.head
    LexerState(state.tokens.tail, current, state.ignoreLocation, state.sourceOffset)
  }

  def parseExpr(): Either[ParseError, (Expr, LexerState)] = {
    parseExprInternal(state)
  }

  private def parseExprInternal(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    for {
      (first, firstState) <- parseAtom(state)
      (withSuffix, afterSuffix) <- parseAtomSuffix(first, firstState)
      (result, resultState) <- parseOpSeq(withSuffix, afterSuffix)
    } yield (result, resultState)
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.Identifier(name, pos)) if isOperator(name.map(_.text).mkString) =>
        val nameStr = name.map(_.text).mkString
        val meta = createMeta(pos, pos)
        val afterOp = advance(state)
        parseAtom(afterOp).map { case (right, afterRight) =>
          (OpSeq(Vector(Identifier(nameStr, meta), right), None), afterRight)
        }
      case Right(Token.Operator(op, pos)) =>
        val meta = createMeta(pos, pos)
        val afterOp = advance(state)
        parseAtom(afterOp).map { case (right, afterRight) =>
          (OpSeq(Vector(Identifier(op, meta), right), None), afterRight)
        }
      case Right(Token.LBrace(_)) =>
        parseObjectClauses(state)
      case Right(Token.LBracket(_)) =>
        parseList(state)
      case Right(Token.LParen(_)) =>
        parseTuple(state)
      case Right(Token.Identifier(name, pos)) =>
        val nameStr = name.map(_.text).mkString
        val meta = createMeta(pos, pos)
        Right((Identifier(nameStr, meta), advance(state)))
      case Right(Token.IntegerLiteral(value, pos)) =>
        val meta = createMeta(pos, pos)
        val intValue = if (value.startsWith("0x") || value.startsWith("0X")) {
          BigInt(value.substring(2), 16)
        } else if (value.startsWith("0b") || value.startsWith("0B")) {
          BigInt(value.substring(2), 2)
        } else {
          BigInt(value)
        }
        Right((IntegerLiteral(intValue, meta), advance(state)))
      case Right(Token.StringLiteral(value, pos)) =>
        val meta = createMeta(pos, pos)
        Right((StringLiteral(value.map(_.text).mkString, meta), advance(state)))
      case Right(Token.Comment(content, pos)) =>
        parseAtom(advance(state))
      case Right(t) => Left(ParseError("Unexpected token", t.pos))
      case Left(err) => Left(err)
    }
  }

  private def parseAtomSuffix(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    def parseSuffix(current: Expr, currentState: LexerState): Either[ParseError, (Expr, LexerState)] = {
      currentState.current match {
        case Right(Token.Dot(_)) =>
          val afterDot = advance(currentState)
          parseAtom(afterDot).flatMap { case (field, afterField) =>
            val telescope = Vector.empty[MaybeTelescope]
            parseSuffix(DotCall(current, field, telescope, None), afterField)
          }
        case Right(Token.LParen(_)) =>
          parseTelescope(currentState).flatMap { case (telescope, afterTelescope) =>
            val telescopeExpr = telescope match {
              case tuple: Tuple => tuple
              case other => Tuple(Vector(other), None)
            }
            current match {
              case DotCall(expr, field, telescopes, meta) =>
                parseSuffix(DotCall(expr, field, telescopes :+ telescopeExpr, meta), afterTelescope)
              case _ =>
                parseSuffix(FunctionCall(current, telescopeExpr, None), afterTelescope)
            }
          }
        case Right(Token.LBrace(_)) =>
          parseBlock(currentState).flatMap { case (block, afterBlock) =>
            current match {
              case DotCall(expr, field, telescopes, meta) =>
                parseSuffix(DotCall(expr, field, telescopes :+ Tuple(Vector(block), None), meta), afterBlock)
              case _ =>
                parseSuffix(FunctionCall(current, Tuple(Vector(block), None), None), afterBlock)
            }
          }
        case _ => Right((current, currentState))
      }
    }
    parseSuffix(expr, state)
  }

  private def parseOpSeq(first: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    def collectOpSeq(terms: Vector[Expr], currentState: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
      currentState.current match {
        case Right(Token.Identifier(name, pos)) if isOperator(name.map(_.text).mkString) =>
          val nameStr = name.map(_.text).mkString
          val meta = createMeta(pos, pos)
          val afterOp = advance(currentState)
          parseAtom(afterOp).flatMap { case (right, afterRight) =>
            collectOpSeq(terms :+ Identifier(nameStr, meta) :+ right, afterRight)
          }
        case Right(Token.Operator(op, pos)) =>
          val meta = createMeta(pos, pos)
          val afterOp = advance(currentState)
          parseAtom(afterOp).flatMap { case (right, afterRight) =>
            collectOpSeq(terms :+ Identifier(op, meta) :+ right, afterRight)
          }
        case _ => Right((terms, currentState))
      }
    }

    collectOpSeq(Vector(first), state).map { case (terms, finalState) =>
      if (terms.length == 1) (first, finalState)
      else (OpSeq(terms, None), finalState)
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    boundary {
      state.current match {
        case Right(Token.LBrace(_)) =>
          val afterBrace = advance(state)
          afterBrace.current match {
            case Right(Token.RBrace(_)) =>
              Right((Block(Vector.empty, None, None), advance(afterBrace)))
            case Right(Token.LBrace(_)) =>
              parseObject(state).map { case (obj, objState) => 
                (Block(Vector.empty, Some(obj), None), objState) 
              }
            case _ =>
              parseExprInternal(afterBrace).flatMap { case (first, firstState) =>
                var exprs = Vector[Expr]()
                var result: Option[Expr] = None
                var currentState = firstState

                def parseNextExpr(): Either[ParseError, Unit] = {
                  parseExprInternal(currentState) match {
                    case Right((expr, exprState)) =>
                      currentState = exprState
                      currentState.current match {
                        case Right(Token.RBrace(_)) =>
                          result = Some(expr)
                          currentState = advance(currentState)
                          Right(())
                        case Right(Token.Semicolon(_)) =>
                          exprs = exprs :+ expr
                          currentState = advance(currentState)
                          Right(())
                        case Right(t) => Left(ParseError("Expected ';' or '}'", t.pos))
                        case Left(err) => break(Left(err))
                      }
                    case Left(err) => Left(err)
                  }
                }

                currentState.current match {
                  case Right(Token.RBrace(_)) =>
                    result = Some(first)
                    currentState = advance(currentState)
                    Right((Block(exprs, result, None), currentState))
                  case Right(Token.Semicolon(_)) =>
                    exprs = exprs :+ first
                    currentState = advance(currentState)
                    var done = false
                    while (!done) {
                      currentState.current match {
                        case Right(Token.RBrace(_)) =>
                          done = true
                          currentState = advance(currentState)
                        case Right(Token.Comment(_, _)) =>
                          currentState = advance(currentState)
                        case _ =>
                          parseNextExpr() match {
                            case Right(_) => ()
                            case Left(err) => break(Left(err))
                          }
                      }
                    }
                    Right((Block(exprs, result, None), currentState))
                  case Right(t) => Left(ParseError("Expected ';' or '}'", t.pos))
                  case Left(err) => Left(err)
                }
              }
          }
        case Right(t) => Left(ParseError("Expected '{'", t.pos))
        case Left(err) => Left(err)
      }
    }
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      state.current match {
        case Right(Token.LParen(startPos)) =>
          val afterParen = advance(state)
          afterParen.current match {
            case Right(Token.RParen(_)) =>
              Right((Tuple(Vector.empty, None), advance(afterParen)))
            case _ =>
              parseTupleItem(afterParen).flatMap { case (first, firstState) =>
                var exprs = Vector(first)
                var currentState = firstState
                var done = false
                
                while (!done) {
                  currentState.current match {
                    case Right(Token.RParen(_)) =>
                      done = true
                      currentState = advance(currentState)
                    case Right(Token.Comma(_)) =>
                      currentState = advance(currentState)
                      parseTupleItem(currentState) match {
                        case Right((expr, exprState)) =>
                          exprs = exprs :+ expr
                          currentState = exprState
                        case Left(err) => break(Left(err))
                      }
                    case Right(t) => break(Left(ParseError("Expected ',' or ')'", t.pos)))
                    case Left(err) => break(Left(err))
                  }
                }
                Right((Tuple(exprs, None), currentState))
              }
          }
        case Right(t) => Left(ParseError("Expected '('", t.pos))
        case Left(err) => Left(err)
      }
    }
  }

  private def parseTupleItem(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      parseAtom(state).flatMap { case (expr, exprState) =>
        exprState.current match {
          case Right(Token.Colon(_)) =>
            val colonState = advance(exprState)
            parseExprInternal(colonState).flatMap { case (typeExpr, typeState) =>
              typeState.current match {
                case Right(Token.Equal(_)) =>
                  val eqState = advance(typeState)
                  parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
                    (OpSeq(Vector(expr, Identifier(":", None), typeExpr, Identifier("=", None), defaultExpr), None), defaultState)
                  }
                case Right(Token.Operator("*", _)) =>
                  val starState = advance(typeState)
                  Right((OpSeq(Vector(expr, Identifier(":", None), typeExpr, Identifier("*", None)), None), starState))
                case _ => 
                  Right((OpSeq(Vector(expr, Identifier(":", None), typeExpr), None), typeState))
              }
            }
          case Right(Token.Equal(_)) =>
            val eqState = advance(exprState)
            parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
              (OpSeq(Vector(expr, Identifier("=", None), defaultExpr), None), defaultState)
            }
          case Right(Token.Operator("*", _)) =>
            val starState = advance(exprState)
            Right((OpSeq(Vector(expr, Identifier("*", None)), None), starState))
          case _ => Right((expr, exprState))
        }
      }
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      state.current match {
        case Right(Token.LBracket(pos)) =>
          val nextState = advance(state)
          var current = nextState
          var terms = Vector.empty[Expr]
          var done = false

          while (!done) {
            current.current match {
              case Right(Token.RBracket(_)) =>
                current = advance(current)
                done = true
              case Right(Token.Comment(_, _)) =>
                current = advance(current)
              case Right(Token.Comma(_)) =>
                current = advance(current)
              case _ =>
                parseExprInternal(current) match {
                  case Right((expr, exprState)) =>
                    terms = terms :+ expr
                    current = exprState
                    current.current match {
                      case Right(Token.Comma(_)) =>
                        current = advance(current)
                      case Right(Token.RBracket(_)) =>
                        current = advance(current)
                        done = true
                      case Right(t) => break(Left(ParseError("Expected ',' or ']'", t.pos)))
                      case Left(err) => break(Left(err))
                    }
                  case Left(err) => break(Left(err))
                }
            }
          }

          Right((ListExpr(terms, None), current))
        case Right(t) => Left(ParseError("Expected '['", t.pos))
        case Left(err) => Left(err)
      }
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    boundary {
      var current = state
      var exprs = Vector.empty[Expr]
      var done = false

      while (!done) {
        current.current match {
          case Right(Token.EOF(_)) =>
            done = true
          case Right(_) =>
            parseExprInternal(current) match {
              case Right((expr, exprState)) =>
                exprs = exprs :+ expr
                current = exprState
                current.current match {
                  case Right(Token.Semicolon(_)) =>
                    current = advance(current)
                  case Right(Token.EOF(_)) =>
                    done = true
                  case _ =>
                    break(Left(ParseError("Expected ';' or EOF", current.current.fold(_.pos, _.pos))))
                }
              case Left(err) => break(Left(err))
            }
          case Left(err) => break(Left(err))
        }
      }

      Right((exprs, current))
    }
  }

  private def parseObject(state: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
    boundary {
      state.current match {
        case Right(Token.LBrace(_)) =>
          var current = advance(state)
          var clauses = Vector[ObjectClause]()
          var done = false

          while (!done) {
            current.current match {
              case Right(Token.RBrace(_)) =>
                current = advance(current)
                done = true
              case Right(Token.Comment(_, _)) =>
                current = advance(current)
              case Right(Token.Semicolon(_)) =>
                current = advance(current)
              case Right(Token.Comma(_)) =>
                current = advance(current)
              case Right(Token.SymbolLiteral(name, pos)) =>
                val meta = createMeta(pos, pos)
                val afterKey = advance(current)
                afterKey.current match {
                  case Right(Token.Equal(_)) =>
                    val afterEqual = advance(afterKey)
                    parseExprInternal(afterEqual) match {
                      case Right((value, valueState)) =>
                        clauses = clauses :+ ObjectExprClause(Identifier(name, meta), value)
                        current = valueState
                      case Left(err) => break(Left(err))
                    }
                  case Right(Token.Operator("=>", _)) =>
                    val afterArrow = advance(afterKey)
                    parseExprInternal(afterArrow) match {
                      case Right((value, valueState)) =>
                        clauses = clauses :+ ObjectExprClauseOnValue(SymbolLiteral(name, meta), value)
                        current = valueState
                      case Left(err) => break(Left(err))
                    }
                  case Right(t) => break(Left(ParseError("Expected '=' or '=>' in object field", t.pos)))
                  case Left(err) => break(Left(err))
                }
              case _ =>
                parseExprInternal(current) match {
                  case Right((key, keyState)) =>
                    keyState.current match {
                      case Right(Token.Equal(_)) =>
                        val afterEqual = advance(keyState)
                        parseExprInternal(afterEqual) match {
                          case Right((value, valueState)) =>
                            key match {
                              case id: Identifier =>
                                clauses = clauses :+ ObjectExprClause(id, value)
                                current = valueState
                              case _ => break(Left(ParseError("Expected identifier as object field key", keyState.current.fold(_.pos, _.pos))))
                            }
                          case Left(err) => break(Left(err))
                        }
                      case Right(Token.Operator("=>", _)) =>
                        val afterArrow = advance(keyState)
                        parseExprInternal(afterArrow) match {
                          case Right((value, valueState)) =>
                            clauses = clauses :+ ObjectExprClauseOnValue(key, value)
                            current = valueState
                          case Left(err) => break(Left(err))
                        }
                      case Right(t) => break(Left(ParseError("Expected '=' or '=>' in object field", t.pos)))
                      case Left(err) => break(Left(err))
                    }
                  case Left(err) => break(Left(err))
                }
            }
          }

          Right((ObjectExpr(clauses, None), current))
        case Right(t) => Left(ParseError("Expected '{' for object", t.pos))
        case Left(err) => Left(err)
      }
    }
  }

  private def parseObjectClauses(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(_)) =>
        val afterBrace = advance(state)
        afterBrace.current match {
          case Right(Token.RBrace(_)) =>
            Right((ObjectExpr(Vector.empty, None), advance(afterBrace)))
          case _ =>
            parseObjectClausesRec(afterBrace, Vector.empty).flatMap { case (clauses, afterClauses) =>
              afterClauses.current match {
                case Right(Token.RBrace(_)) =>
                  Right((ObjectExpr(clauses, None), advance(afterClauses)))
                case Right(t) => Left(ParseError("Expected '}'", t.pos))
                case Left(err) => Left(err)
              }
            }
        }
      case Right(t) => Left(ParseError("Expected '{'", t.pos))
      case Left(err) => Left(err)
    }
  }

  private def parseObjectClausesRec(state: LexerState, acc: Vector[ObjectClause]): Either[ParseError, (Vector[ObjectClause], LexerState)] = {
    state.current match {
      case Right(Token.RBrace(_)) => Right((acc, state))
      case Right(Token.Comma(_)) =>
        val afterComma = advance(state)
        parseObjectClausesRec(afterComma, acc)
      case Right(Token.Comment(_, _)) =>
        val afterComment = advance(state)
        parseObjectClausesRec(afterComment, acc)
      case _ =>
        parseExprInternal(state).flatMap { case (key, afterKey) =>
          afterKey.current match {
            case Right(Token.Equal(_)) =>
              val afterEquals = advance(afterKey)
              parseExprInternal(afterEquals).flatMap { case (value, afterValue) =>
                key match {
                  case id: Identifier =>
                    val clause = ObjectExprClause(id, value)
                    parseObjectClausesRec(afterValue, acc :+ clause)
                  case _ =>
                    val clause = ObjectExprClauseOnValue(key, value)
                    parseObjectClausesRec(afterValue, acc :+ clause)
                }
              }
            case Right(t) => Left(ParseError("Expected '=' in object field", t.pos))
            case Left(err) => Left(err)
          }
        }
    }
  }

  private def isOperator(name: String): Boolean = {
    name.matches("[!@#$%^&*+\\-=<>/?|~:]+") || 
    name == "and" || name == "or" || name == "not" || 
    name == "->" || name == "=>" || name == "if" || name == "then" || name == "else" || name == "match"
  }

  private def parseTelescope(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LParen(startPos)) =>
        val afterParen = advance(state)
        afterParen.current match {
          case Right(Token.RParen(_)) =>
            Right((Tuple(Vector.empty, None), advance(afterParen)))
          case _ =>
            parseTupleItem(afterParen).flatMap { case (first, firstState) =>
              var exprs = Vector(first)
              var currentState = firstState
              var done = false
              
              while (!done) {
                currentState.current match {
                  case Right(Token.RParen(_)) =>
                    done = true
                    currentState = advance(currentState)
                  case Right(Token.Comma(_)) =>
                    currentState = advance(currentState)
                    parseTupleItem(currentState) match {
                      case Right((expr, exprState)) =>
                        exprs = exprs :+ expr
                        currentState = exprState
                      case Left(err) => return Left(err)
                    }
                  case Right(t) => return Left(ParseError("Expected ',' or ')'", t.pos))
                  case Left(err) => return Left(err)
                }
              }
              Right((Tuple(exprs, None), currentState))
            }
        }
      case Right(t) => Left(ParseError("Expected '('", t.pos))
      case Left(err) => Left(err)
    }
  }
}