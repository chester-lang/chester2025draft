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
    state.current match {
      case Right(Token.Comment(_, _)) =>
        parseExprInternal(advance(state))
      case Right(Token.Identifier(name, pos)) =>
        val op = name.map(_.text).mkString
        if (IdentifierRules.strIsOperator(op)) {
          val nextState = advance(state)
          parseAtom(nextState).map { case (expr, exprState) =>
            val endPos = expr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(pos)
            val meta = createMeta(pos, endPos)
            (OpSeq(Vector(Identifier(op, createMeta(pos, pos)), expr), meta), exprState)
          }
        } else {
          parseAtom(state).flatMap { case (first, firstState) =>
            parseOpSeq(first, firstState)
          }
        }
      case _ =>
        parseAtom(state).flatMap { case (first, firstState) =>
          parseOpSeq(first, firstState)
        }
    }
  }

  private def parseOpSeq(first: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    var terms = Vector(first)
    var done = false
    val startPos = first.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(first match {
      case Identifier(_, meta) => meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(Pos(WithUTF16(0,0), 0, WithUTF16(0,0)))
      case _ => Pos(WithUTF16(0,0), 0, WithUTF16(0,0))
    })

    while (!done) {
      current.current match {
        case Right(Token.Identifier(name, pos)) =>
          val op = name.map(_.text).mkString
          if (op == "if" || op == "then" || op == "else" || op == "and" || op == "or" || op == "not" || op == "->" || op == "val" || op == "getthen") {
            val operator = Identifier(op, createMeta(pos, pos))
            current = advance(current)
            parseAtom(current) match {
              case Right((expr, exprState)) =>
                terms = terms :+ operator :+ expr
                current = exprState
              case Left(err) => return Left(err)
            }
          } else if (IdentifierRules.strIsOperator(op)) {
            val operator = Identifier(op, createMeta(pos, pos))
            current = advance(current)
            parseAtom(current) match {
              case Right((expr, exprState)) =>
                terms = terms :+ operator :+ expr
                current = exprState
              case Left(err) => return Left(err)
            }
          } else {
            done = true
          }
        case _ =>
          done = true
      }
    }

    if (terms.length == 1) {
      Right((terms.head, current))
    } else {
      val endPos = terms.last.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(terms.last match {
        case Identifier(_, meta) => meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(Pos(WithUTF16(0,0), 0, WithUTF16(0,0)))
        case _ => Pos(WithUTF16(0,0), 0, WithUTF16(0,0))
      })
      val meta = createMeta(startPos, endPos)
      Right((OpSeq(terms, meta), current))
    }
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.Comment(_, _)) =>
        parseAtom(advance(state))
      case Right(Token.LParen(_)) =>
        parseTuple(state)
      case Right(Token.LBrace(_)) =>
        parseBlock(state)
      case Right(Token.LBracket(_)) =>
        parseList(state)
      case Right(Token.Identifier(name, pos)) =>
        val nextState = advance(state)
        val identifier = Identifier(name.map(_.text).mkString, createMeta(pos, pos))
        nextState.current match {
          case Right(Token.LParen(_)) =>
            parseTuple(nextState).map { case (tuple: Tuple, tupleState) =>
              (FunctionCall(identifier, tuple, None), tupleState)
            }
          case Right(Token.LBrace(_)) =>
            parseBlock(nextState).map { case (block, blockState) =>
              (Block(Vector(), Some(block), None), blockState)
            }
          case Right(Token.LBracket(_)) =>
            parseList(nextState).map { case (list: ListExpr, listState) =>
              (FunctionCall(identifier, Tuple(list.terms, None), None), listState)
            }
          case _ => Right((identifier, nextState))
        }
      case Right(Token.IntegerLiteral(value, pos)) =>
        Right((IntegerLiteral(BigInt(value), createMeta(pos, pos)), advance(state)))
      case Right(Token.StringLiteral(value, pos)) =>
        Right((StringLiteral(value.map(_.text).mkString, createMeta(pos, pos)), advance(state)))
      case Right(t) => Left(ParseError("Unexpected token", t.pos))
      case Left(err) => Left(err)
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
              parseExprInternal(current) match {
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
                    case Right(Token.Identifier(name, pos)) if name.map(_.text).mkString == "*" =>
                      current = advance(current)
                      terms = terms.init :+ Tuple(Vector(terms.last), None)
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

  private def parseTupleItem(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    parseExprInternal(state).flatMap { case (expr, exprState) =>
      exprState.current match {
        case Right(Token.Identifier(name, pos)) if name.map(_.text).mkString == ":" =>
          val colonState = advance(exprState)
          parseExprInternal(colonState).flatMap { case (typeExpr, typeState) =>
            typeState.current match {
              case Right(Token.Identifier(name, pos)) if name.map(_.text).mkString == "=" =>
                val eqState = advance(typeState)
                parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
                  val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(pos)
                  val endPos = defaultExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(pos)
                  val meta = createMeta(startPos, endPos)
                  (OpSeq(Vector(expr, Identifier(":", createMeta(pos, pos)), typeExpr, Identifier("=", createMeta(pos, pos)), defaultExpr), meta), defaultState)
                }
              case _ => 
                val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(pos)
                val endPos = typeExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(pos)
                val meta = createMeta(startPos, endPos)
                Right((OpSeq(Vector(expr, Identifier(":", createMeta(pos, pos)), typeExpr), meta), typeState))
            }
          }
        case Right(Token.Identifier(name, pos)) if name.map(_.text).mkString == "=" =>
          val eqState = advance(exprState)
          parseExprInternal(eqState).map { case (defaultExpr, defaultState) =>
            val startPos = expr.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(pos)
            val endPos = defaultExpr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(pos)
            val meta = createMeta(startPos, endPos)
            (OpSeq(Vector(expr, Identifier("=", createMeta(pos, pos)), defaultExpr), meta), defaultState)
          }
        case _ => Right((expr, exprState))
      }
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
}