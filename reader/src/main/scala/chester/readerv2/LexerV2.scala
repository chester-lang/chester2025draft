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
    boundary {
      parseAtom(state).flatMap { case (first, firstState) =>
        parseOpSeq(first, firstState)
      }
    }
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      state.current match {
        case Right(token) =>
          token match {
            case Token.Operator(op, pos) =>
              val meta = createMeta(pos, pos)
              val opExpr = Identifier(op, meta)
              val nextState = advance(state)
              nextState.current match {
                case Right(Token.LParen(_)) =>
                  parseTuple(nextState).flatMap { case (tuple, tupleState) =>
                    tuple match {
                      case t: Tuple =>
                        tupleState.current match {
                          case Right(Token.LBrace(_)) =>
                            parseBlock(tupleState).map { case (block, blockState) =>
                              (FunctionCall(FunctionCall(opExpr, t, None), Tuple(Vector(block), None), None), blockState)
                            }
                          case _ =>
                            Right((FunctionCall(opExpr, t, None), tupleState))
                        }
                      case _ => Left(ParseError("Expected tuple arguments", tupleState.current.fold(_.pos, _.pos)))
                    }
                  }
                case Right(Token.LBrace(_)) =>
                  parseBlock(nextState).map { case (block, blockState) =>
                    (FunctionCall(opExpr, Tuple(Vector(block), None), None), blockState)
                  }
                case _ =>
                  parseAtomBase(nextState).map { case (rightExpr, rightState) =>
                    (OpSeq(Vector(opExpr, rightExpr), None), rightState)
                  }
              }
            case Token.Identifier(name, pos) =>
              val meta = createMeta(pos, pos)
              val opExpr = Identifier(name.map(_.text).mkString, meta)
              val nextState = advance(state)
              nextState.current match {
                case Right(Token.LParen(_)) =>
                  parseTuple(nextState).flatMap { case (tuple, tupleState) =>
                    tuple match {
                      case t: Tuple =>
                        tupleState.current match {
                          case Right(Token.LBrace(_)) =>
                            parseBlock(tupleState).map { case (block, blockState) =>
                              (FunctionCall(FunctionCall(opExpr, t, None), Tuple(Vector(block), None), None), blockState)
                            }
                          case _ =>
                            Right((FunctionCall(opExpr, t, None), tupleState))
                        }
                      case _ => Left(ParseError("Expected tuple arguments", tupleState.current.fold(_.pos, _.pos)))
                    }
                  }
                case Right(Token.LBrace(_)) =>
                  parseBlock(nextState).map { case (block, blockState) =>
                    (FunctionCall(opExpr, Tuple(Vector(block), None), None), blockState)
                  }
                case _ =>
                  parseAtomBase(nextState).map { case (rightExpr, rightState) =>
                    (OpSeq(Vector(opExpr, rightExpr), None), rightState)
                  }
              }
            case _ => parseAtomBase(state)
          }
        case Left(err) => Left(err)
      }
    }
  }

  private def parseAtomBase(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(startPos)) =>
        parseObject(state)
      case Right(Token.LParen(startPos)) =>
        parseTuple(state)
      case Right(Token.LBracket(startPos)) =>
        parseList(state)
      case Right(Token.StringLiteral(value, startPos)) =>
        val meta = createMeta(startPos, startPos)
        Right((StringLiteral(value.map(_.text).mkString, meta), advance(state)))
      case Right(Token.IntegerLiteral(value, startPos)) =>
        val meta = createMeta(startPos, startPos)
        val intValue = if (value.startsWith("0x") || value.startsWith("0X")) {
          BigInt(value.substring(2), 16)
        } else if (value.startsWith("0b") || value.startsWith("0B")) {
          BigInt(value.substring(2), 2)
        } else {
          BigInt(value)
        }
        Right((IntegerLiteral(intValue, meta), advance(state)))
      case Right(Token.RationalLiteral(value, startPos)) =>
        val meta = createMeta(startPos, startPos)
        Right((RationalLiteral(Rational(value), meta), advance(state)))
      case Right(Token.SymbolLiteral(value, startPos)) =>
        val meta = createMeta(startPos, startPos)
        Right((SymbolLiteral(value, meta), advance(state)))
      case Right(Token.Identifier(name, startPos)) =>
        val meta = createMeta(startPos, startPos)
        val expr = Identifier(name.map(_.text).mkString, meta)
        val nextState = advance(state)
        parseDotOrCall(expr, nextState)
      case Right(t) => Left(ParseError("Unexpected token", t.pos))
      case Left(err) => Left(err)
    }
  }

  private def parseDotOrCall(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LParen(_)) =>
        parseTuple(state).flatMap { case (tuple, argsState) =>
          tuple match {
            case t: Tuple =>
              argsState.current match {
                case Right(Token.LBrace(_)) =>
                  parseBlock(argsState).map { case (block, blockState) =>
                    (FunctionCall(FunctionCall(expr, t, None), Tuple(Vector(block), None), None), blockState)
                  }
                case _ =>
                  Right((FunctionCall(expr, t, None), argsState))
              }
            case other => Left(ParseError("Expected tuple arguments", argsState.current.fold(_.pos, _.pos)))
          }
        }
      case Right(Token.LBrace(_)) =>
        parseBlock(state).map { case (block, blockState) =>
          (FunctionCall(expr, Tuple(Vector(block), None), None), blockState)
        }
      case Right(Token.Dot(_)) =>
        val afterDot = advance(state)
        afterDot.current match {
          case Right(Token.Identifier(name, _)) =>
            val afterId = advance(afterDot)
            parseDotOrCall(DotCall(expr, Identifier(name.map(_.text).mkString, None), Vector(), None), afterId)
          case Right(t) => Left(ParseError("Expected identifier after '.'", t.pos))
          case Left(err) => Left(err)
        }
      case _ => Right((expr, state))
    }
  }

  private def parseOpSeq(first: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      def collectOperands(state: LexerState, terms: Vector[Expr] = Vector(first)): Either[ParseError, (Vector[Expr], LexerState)] = {
        state.current match {
          case Right(Token.Identifier(name, pos)) =>
            val nameStr = name.map(_.text).mkString
            val meta = createMeta(pos, pos)
            val afterOp = advance(state)
            parseAtom(afterOp).flatMap { case (next, nextState) =>
              collectOperands(nextState, terms :+ Identifier(nameStr, meta) :+ next)
            }
          case Right(Token.Operator(op, pos)) =>
            val meta = createMeta(pos, pos)
            val afterOp = advance(state)
            parseAtom(afterOp).flatMap { case (next, nextState) =>
              collectOperands(nextState, terms :+ Identifier(op, meta) :+ next)
            }
          case Right(Token.LBrace(_)) =>
            parseBlock(state).flatMap { case (block, blockState) =>
              collectOperands(blockState, terms :+ block)
            }
          case _ => Right((terms, state))
        }
      }

      collectOperands(state).map { case (terms, finalState) =>
        if (terms.size == 1) (terms.head, finalState)
        else (OpSeq(terms, None), finalState)
      }
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
                var exprs = Vector(first)
                var currentState = firstState
                var done = false
                
                while (!done) {
                  currentState.current match {
                    case Right(Token.RBrace(_)) =>
                      done = true
                      currentState = advance(currentState)
                    case Right(Token.Semicolon(_)) =>
                      currentState = advance(currentState)
                      parseExprInternal(currentState) match {
                        case Right((expr, exprState)) =>
                          exprs = exprs :+ expr
                          currentState = exprState
                        case Left(err) => break(Left(err))
                      }
                    case Right(t) => break(Left(ParseError("Expected ';' or '}'", t.pos)))
                    case Left(err) => break(Left(err))
                  }
                }
                Right((Block(exprs, None, None), currentState))
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
              parseExprInternal(afterParen).flatMap { case (first, firstState) =>
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
                      parseExprInternal(currentState) match {
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
              case _ =>
                parseAtomBase(current) match {
                  case Right((key, keyState)) =>
                    keyState.current match {
                      case Right(Token.Equal(_)) =>
                        val afterEqual = advance(keyState)
                        parseExprInternal(afterEqual) match {
                          case Right((value, valueState)) =>
                            key match {
                              case Identifier(name, meta) =>
                                clauses = clauses :+ ObjectExprClause(Identifier(name, meta), value)
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
}