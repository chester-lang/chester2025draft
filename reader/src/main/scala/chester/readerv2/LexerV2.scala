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
                parseTuple(nextState).flatMap { case (tuple, tupleState) =>
                  tupleState.current match {
                    case Right(Token.LBrace(_)) =>
                      parseBlock(tupleState).map { case (block, blockState) =>
                        (FunctionCall(FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), Tuple(Vector(block), None), None), blockState)
                      }
                    case _ =>
                      Right((FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), tupleState))
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
                  tupleState.current match {
                    case Right(Token.LBrace(_)) =>
                      parseBlock(tupleState).map { case (block, blockState) =>
                        (FunctionCall(FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), Tuple(Vector(block), None), None), blockState)
                      }
                    case _ =>
                      Right((FunctionCall(opExpr, tuple.asInstanceOf[Tuple], None), tupleState))
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
        parseTuple(state).flatMap { case (args: Tuple, argsState) =>
          argsState.current match {
            case Right(Token.LBrace(_)) =>
              parseBlock(argsState).map { case (block, blockState) =>
                (FunctionCall(FunctionCall(expr, args, None), Tuple(Vector(block), None), None), blockState)
              }
            case _ =>
              Right((FunctionCall(expr, args, None), argsState))
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
        case Right(Token.Identifier(name, pos)) =>
          val nameStr = name.map(_.text).mkString
          val meta = createMeta(pos, pos)
          val afterOp = advance(state)
          parseAtomBase(afterOp).flatMap { case (next, nextState) =>
            collectOperands(nextState, terms :+ Identifier(nameStr, meta) :+ next)
          }
        case Right(Token.Operator(op, pos)) =>
          val meta = createMeta(pos, pos)
          val afterOp = advance(state)
          parseAtomBase(afterOp).flatMap { case (next, nextState) =>
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

  private def isOperator(name: String): Boolean = {
    name match {
      case "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||" | "!" | "->" | "=>" => true
      case _ => false
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(_)) =>
        var current = advance(state)
        var statements = Vector[Expr]()
        var result: Option[Expr] = None
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
            case _ =>
              parseExprInternal(current) match {
                case Right((expr, exprState)) =>
                  exprState.current match {
                    case Right(Token.Semicolon(_)) =>
                      statements = statements :+ expr
                      current = advance(exprState)
                    case Right(Token.RBrace(_)) =>
                      result = Some(expr)
                      current = exprState
                      done = true
                    case Right(Token.Equal(_)) | Right(Token.Operator("=>", _)) =>
                      // Only treat as object if we haven't seen any statements yet and this is the first expression
                      if (statements.isEmpty && result.isEmpty && current == advance(state)) {
                        return parseObject(state).map { case (obj, objState) => (Block(Vector(), Some(obj), None), objState) }
                      } else {
                        return Left(ParseError("Unexpected '=' or '=>' in block", exprState.current.fold(_.pos, _.pos)))
                      }
                    case Right(t) =>
                      result = Some(expr)
                      current = exprState
                      done = true
                    case Left(err) => return Left(err)
                  }
                case Left(err) => return Left(err)
              }
          }
        }

        Right((Block(statements, result, None), current))
      case Right(t) => Left(ParseError("Expected '{' for block", t.pos))
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
              case _ => 
                Right((OpSeq(Vector(expr, Identifier(":", None), typeExpr), None), typeState))
            }
          }
        case Right(Token.Equal(_)) =>
          val eqState = advance(exprState)
          parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
            (OpSeq(Vector(expr, Identifier("=", None), defaultExpr), None), defaultState)
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

  private def parseObject(state: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
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
                            case _ => return Left(ParseError("Expected identifier as object field key", keyState.current.fold(_.pos, _.pos)))
                          }
                        case Left(err) => return Left(err)
                      }
                    case Right(Token.Operator("=>", _)) =>
                      val afterArrow = advance(keyState)
                      parseExprInternal(afterArrow) match {
                        case Right((value, valueState)) =>
                          clauses = clauses :+ ObjectExprClauseOnValue(key, value)
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