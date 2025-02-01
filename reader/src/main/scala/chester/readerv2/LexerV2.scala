package chester.readerv2

import chester.error.{Reporter, Pos, SourcePos, RangeInFile}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.*
import chester.syntax.IdentifierRules
import chester.utils.WithUTF16
import spire.math.Rational
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.numeric.Positive0

case class LexerState(
  tokens: TokenStream,
  current: Either[ParseError, Token],
  ignoreLocation: Boolean,
  sourceOffset: SourceOffset
)

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, reporter: Reporter[ParseError], ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, reporter, ignoreLocation)
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, reporter: Reporter[ParseError], ignoreLocation: Boolean) {
  private var state: LexerState = {
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
    parseAtom(state).flatMap { case (first, firstState) =>
      parseOpSeq(first, firstState)
    }
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(token) =>
        token match {
          case Token.Operator(op, pos) =>
            val meta = createMeta(pos, pos)
            val opExpr = Identifier(op, meta)
            val nextState = advance(state)
            nextState.current match {
              case Right(Token.LParen(_)) =>
                parseTuple(nextState).map { case (tuple, tupleState) =>
                  (FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), tupleState)
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
                parseTuple(nextState).map { case (tuple, tupleState) =>
                  (FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), tupleState)
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

  private def parseAtomBase(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(startPos)) =>
        // Try parsing as an object first
        parseObject(state) match {
          case Right(result) => Right(result)
          case Left(_) => 
            // If object parsing fails, try parsing as a block
            parseBlock(state)
        }
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
        val doubleValue = value.toDouble
        Right((RationalLiteral(doubleValue, meta), advance(state)))
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
      case Right(Token.LBracket(_)) =>
        parseList(state).flatMap { case (generics, genericsState) =>
          val withGenerics = FunctionCall(expr, generics.asInstanceOf[ListExpr], None)
          parseDotOrCall(withGenerics, genericsState)
        }
      case Right(Token.Dot(_)) =>
        val afterDot = advance(state)
        afterDot.current match {
          case Right(token) =>
            token match {
              case Token.Operator(op, pos) =>
                val field = Identifier(op, createMeta(pos, pos))
                val afterField = advance(afterDot)
                parseArgLists(afterField).flatMap { case (args, finalState) =>
                  val dotCall = DotCall(expr, field, args, None)
                  parseDotOrCall(dotCall, finalState)
                }
              case Token.Identifier(name, pos) =>
                val field = Identifier(name.map(_.text).mkString, createMeta(pos, pos))
                val afterField = advance(afterDot)
                parseArgLists(afterField).flatMap { case (args, finalState) =>
                  val dotCall = DotCall(expr, field, args, None)
                  parseDotOrCall(dotCall, finalState)
                }
              case t => Left(ParseError("Expected identifier or operator after '.'", t.pos))
            }
          case Left(err) => Left(err)
        }
      case Right(Token.LParen(_)) =>
        parseTuple(state).flatMap { case (tuple, tupleState) =>
          val call = FunctionCall(expr, tuple.asInstanceOf[Tuple], None)
          parseDotOrCall(call, tupleState)
        }
      case Right(Token.LBrace(_)) =>
        parseBlock(state).flatMap { case (block, blockState) =>
          val call = FunctionCall(expr, Tuple(Vector(block), None), None)
          parseDotOrCall(call, blockState)
        }
      case _ => Right((expr, state))
    }
  }

  private def parseArgLists(state: LexerState, args: Vector[Tuple] = Vector.empty): Either[ParseError, (Vector[Tuple], LexerState)] = {
    state.current match {
      case Right(Token.LParen(_)) =>
        parseTuple(state).flatMap { case (tuple, tupleState) =>
          parseArgLists(tupleState, args :+ tuple.asInstanceOf[Tuple])
        }
      case Right(Token.LBrace(_)) =>
        parseBlock(state).flatMap { case (block, blockState) =>
          parseArgLists(blockState, args :+ Tuple(Vector(block), None))
        }
      case _ =>
        Right((args, state))
    }
  }

  private def parseOpSeq(first: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    def collectOperands(state: LexerState, terms: Vector[Expr] = Vector(first)): Either[ParseError, (Vector[Expr], LexerState)] = {
      state.current match {
        case Right(token) =>
          token match {
            case Token.Operator(op, pos) =>
              val opMeta = createMeta(pos, pos)
              val opExpr = Identifier(op, opMeta)
              val afterOpState = advance(state)
              
              parseAtom(afterOpState).flatMap { case (rightExpr, rightState) =>
                collectOperands(rightState, terms :+ opExpr :+ rightExpr)
              }
              
            case Token.Equal(pos) =>
              val opMeta = createMeta(pos, pos)
              val opExpr = Identifier("=", opMeta)
              val afterOpState = advance(state)
              
              parseAtom(afterOpState).flatMap { case (rightExpr, rightState) =>
                collectOperands(rightState, terms :+ opExpr :+ rightExpr)
              }
              
            case Token.Identifier(name, pos) =>
              val opMeta = createMeta(pos, pos)
              val opExpr = Identifier(name.map(_.text).mkString, opMeta)
              val afterOpState = advance(state)
              
              parseAtom(afterOpState).flatMap { case (rightExpr, rightState) =>
                collectOperands(rightState, terms :+ opExpr :+ rightExpr)
              }
              
            case Token.LBrace(_) =>
              parseBlock(state).flatMap { case (block, blockState) =>
                collectOperands(blockState, terms :+ block)
              }
              
            case _ =>
              Right((terms, state))
          }
        case Left(err) => Left(err)
      }
    }

    collectOperands(state).map { case (terms, finalState) =>
      if (terms.size == 1) (terms.head, finalState)
      else (OpSeq(terms, None), finalState)
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(pos)) =>
        val nextState = advance(state)
        var current = nextState
        var statements = Vector[Expr]()
        var result: Option[Expr] = None
        var done = false

        while (!done) {
          current.current match {
            case Right(Token.RBrace(_)) =>
              current = advance(current)
              done = true
            case Right(Token.Comment(text, _)) =>
              current = advance(current)
            case Right(Token.Semicolon(_)) =>
              current = advance(current)
            case _ =>
              parseExprInternal(current) match {
                case Right((expr, exprState)) =>
                  current = exprState
                  current.current match {
                    case Right(Token.Semicolon(_)) =>
                      statements = statements :+ expr
                      current = advance(current)
                    case Right(Token.RBrace(_)) =>
                      result = Some(expr)
                      current = advance(current)
                      done = true
                    case Right(Token.Comment(text, _)) =>
                      current = advance(current)
                    case Right(t) => return Left(ParseError("Expected ';' or '}'", t.pos))
                    case Left(err) => return Left(err)
                  }
                case Left(err) => return Left(err)
              }
          }
        }

        Right((Block(statements, result, None), current))
      case Right(t) => Left(ParseError("Expected '{'", t.pos))
      case Left(err) => Left(err)
    }
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LParen(pos)) =>
        val nextState = advance(state)
        var current = nextState
        var terms = Vector[Expr]()
        var done = false

        while (!done) {
          current.current match {
            case Right(Token.RParen(pos)) =>
              current = advance(current)
              done = true
            case Right(Token.Comment(text, _)) =>
              current = advance(current)
            case _ =>
              parseTupleItem(current) match {
                case Right((term, termState)) =>
                  terms = terms :+ term
                  current = termState
                  current.current match {
                    case Right(Token.Comma(_)) =>
                      current = advance(current)
                    case Right(Token.RParen(_)) =>
                      current = advance(current)
                      done = true
                    case Right(Token.Comment(text, _)) =>
                      current = advance(current)
                    case Right(Token.Operator("*", pos)) =>
                      current = advance(current)
                      terms = terms.init :+ OpSeq(Vector(terms.last, Identifier("*", createMeta(pos, pos))), None)
                    case Right(t) => return Left(ParseError("Expected ',' or ')'", t.pos))
                    case Left(err) => return Left(err)
                  }
                case Left(err) => return Left(err)
              }
          }
        }
        Right((Tuple(terms, None), current))
      case Right(t) => Left(ParseError("Expected '('", t.pos))
      case Left(err) => Left(err)
    }
  }

  private def parseTupleItem(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    parseExprInternal(state).flatMap { case (expr, exprState) =>
      exprState.current match {
        case Right(Token.Colon(_)) =>
          val colonState = advance(exprState)
          parseExprInternal(colonState).flatMap { case (typeExpr, typeState) =>
            typeState.current match {
              case Right(Token.Equal(_)) =>
                val eqState = advance(typeState)
                parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
                  val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(typeState.current.fold(_.pos, _.pos))
                  val endPos = defaultExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(typeState.current.fold(_.pos, _.pos))
                  val meta = createMeta(startPos, endPos)
                  (OpSeq(Vector(expr, Identifier(":", createMeta(startPos, startPos)), typeExpr, Identifier("=", createMeta(startPos, startPos)), defaultExpr), meta), defaultState)
                }
              case _ => 
                val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(typeState.current.fold(_.pos, _.pos))
                val endPos = typeExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(typeState.current.fold(_.pos, _.pos))
                val meta = createMeta(startPos, endPos)
                Right((OpSeq(Vector(expr, Identifier(":", createMeta(startPos, startPos)), typeExpr), meta), typeState))
            }
          }
        case Right(Token.Equal(_)) =>
          val eqState = advance(exprState)
          parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
            val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(defaultState.current.fold(_.pos, _.pos))
            val endPos = defaultExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(defaultState.current.fold(_.pos, _.pos))
            val meta = createMeta(startPos, endPos)
            (OpSeq(Vector(expr, Identifier("=", createMeta(startPos, startPos)), defaultExpr), meta), defaultState)
          }
        case _ => Right((expr, exprState))
      }
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
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
                    case Right(t) => return Left(ParseError("Expected ',' or ']'", t.pos))
                    case Left(err) => return Left(err)
                  }
                case Left(err) => return Left(err)
              }
          }
        }

        Right((ListExpr(terms, None), current))
      case Right(t) => Left(ParseError("Expected '['", t.pos))
      case Left(err) => Left(err)
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
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
                  return Left(ParseError("Expected ';' or EOF", current.current.fold(_.pos, _.pos)))
              }
            case Left(err) => return Left(err)
          }
        case Left(err) => return Left(err)
      }
    }

    Right((exprs, current))
  }

  private def parseObject(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(_)) =>
        val nextState = advance(state)
        var current = nextState
        var clauses = Vector[ObjectExprClause]()
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
              parseAtom(current) match {
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
                            case _ => return Left(ParseError("Expected identifier as object field key", keyState.current.fold(_.pos, _.pos)))
                          }
                        case Left(err) => return Left(err)
                      }
                    case Right(Token.Identifier(arrow, _)) if arrow.map(_.text).mkString == "=>" =>
                      val afterArrow = advance(keyState)
                      parseExprInternal(afterArrow) match {
                        case Right((value, valueState)) =>
                          clauses = clauses :+ ObjectExprClauseOnValue(key, value).asInstanceOf[ObjectExprClause]
                          current = valueState
                        case Left(err) => return Left(err)
                      }
                    case Right(t) => return Left(ParseError("Expected '=' or '=>' in object field", t.pos))
                    case Left(err) => return Left(err)
                  }
                case Left(err) => return Left(err)
              }
          }
        }

        Right((ObjectExpr(clauses, None), current))
      case Right(t) => Left(ParseError("Expected '{' for object", t.pos))
      case Left(err) => Left(err)
    }
  }
}