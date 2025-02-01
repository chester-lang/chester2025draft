package chester.readerv2

import chester.error.{Reporter, Pos, SourcePos, RangeInFile}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.*
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

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(token) => token match {
        case Token.Identifier(name, pos) =>
          val op = name.map(_.text).mkString
          val nextState = advance(state)
          nextState.current match {
            case Right(Token.LParen(startPos)) =>
              parseTuple(advance(nextState)).map { case (tuple: Tuple, tupleState) =>
                val endPos = tuple.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(startPos)
                (FunctionCall(Identifier(op, createMeta(pos, pos)), tuple, createMeta(pos, endPos)), tupleState)
              }
            case Right(Token.LBrace(startPos)) =>
              parseBlock(advance(nextState)).map { case (block, blockState) =>
                val endPos = block.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(startPos)
                val blockTuple = Tuple(Vector(block), createMeta(startPos, endPos))
                (FunctionCall(Identifier(op, createMeta(pos, pos)), blockTuple, createMeta(pos, endPos)), blockState)
              }
            case _ => Right((Identifier(op, createMeta(pos, pos)), nextState))
          }
        case Token.IntegerLiteral(value, pos) =>
          Right((IntegerLiteral(BigInt(value), createMeta(pos, pos)), advance(state)))
        case Token.RationalLiteral(value, pos) =>
          Right((RationalLiteral(Rational(value), createMeta(pos, pos)), advance(state)))
        case Token.StringLiteral(value, pos) =>
          Right((StringLiteral(value.map(_.text).mkString, createMeta(pos, pos)), advance(state)))
        case Token.LParen(startPos) =>
          parseTuple(advance(state)).map { case (tuple, tupleState) =>
            val endPos = tuple.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(startPos)
            (tuple, tupleState)
          }
        case Token.LBrace(startPos) =>
          parseBlock(advance(state)).map { case (block, blockState) =>
            val endPos = block.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(startPos)
            (block, blockState)
          }
        case Token.LBracket(startPos) =>
          parseList(advance(state)).map { case (list, listState) =>
            val endPos = list.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(startPos)
            (list, listState)
          }
        case _ =>
          Left(ParseError("Unexpected token", token.pos))
      }
      case Left(err) => Left(err)
    }
  }

  private def parseExprInternal(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    state.current match {
      case Right(Token.Identifier(name, pos)) =>
        val op = name.map(_.text).mkString
        if (op.matches("[!+\\-~]|not")) {
          val nextState = advance(state)
          parseExprInternal(nextState).map { case (expr, exprState) =>
            val meta = createMeta(pos, expr.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(pos))
            (OpSeq(Vector(Identifier(op, createMeta(pos, pos)), expr), meta), exprState)
          }
        } else {
          parseAtom(state).flatMap { case (first, firstState) =>
            var current = firstState
            var terms = Vector[Expr](first)
            var done = false

            while (!done) {
              current.current match {
                case Right(Token.Identifier(name, pos)) =>
                  val op = name.map(_.text).mkString
                  if (op.matches("[+\\-*/]|and|or|->|=|:|then|else|if")) {
                    current = advance(current)
                    parseAtom(current) match {
                      case Right((next, nextState)) =>
                        terms = terms ++ Vector(Identifier(op, createMeta(pos, pos)), next)
                        current = nextState
                      case Left(err) => return Left(err)
                    }
                  } else {
                    done = true
                  }
                case _ => done = true
              }
            }

            if (terms.length == 1) {
              Right((terms.head, current))
            } else {
              val startPos = terms.head.meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(terms.head match {
                case Identifier(_, meta) => meta.flatMap(_.sourcePos).map(_.range.start).getOrElse(Pos(WithUTF16(0,0), 0, WithUTF16(0,0)))
                case _ => Pos(WithUTF16(0,0), 0, WithUTF16(0,0))
              })
              val endPos = terms.last.meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(terms.last match {
                case Identifier(_, meta) => meta.flatMap(_.sourcePos).map(_.range.end).getOrElse(Pos(WithUTF16(0,0), 0, WithUTF16(0,0)))
                case _ => Pos(WithUTF16(0,0), 0, WithUTF16(0,0))
              })
              val meta = createMeta(startPos, endPos)
              Right((OpSeq(terms, meta), current))
            }
          }
        }
      case _ => parseAtom(state)
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    var statements = Vector.empty[Expr]
    var result: Option[Expr] = None
    val startPos = current.current.fold(_.pos, _.pos)

    def parseStatement(): Either[ParseError, Unit] = {
      current.current match {
        case Right(Token.RBrace(endPos)) =>
          Right(())
        case Right(_) =>
          parseExprInternal(current).flatMap { case (expr, exprState) =>
            current = exprState
            current.current match {
              case Right(Token.RBrace(endPos)) =>
                result = Some(expr)
                Right(())
              case Right(Token.Semicolon(_)) =>
                statements = statements :+ expr
                current = advance(current)
                parseStatement()
              case _ =>
                Left(ParseError("Expected '}' or ';'", current.current.fold(_.pos, _.pos)))
            }
          }
        case Left(err) => Left(err)
      }
    }

    parseStatement().map { _ =>
      val blockState = advance(current)  // Skip the closing brace
      val endPos = blockState.current.fold(_.pos, _.pos)
      val meta = createMeta(startPos, endPos)
      (Block(statements, result, meta), blockState)
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

  private def parseTuple(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    var terms = Vector.empty[Expr]
    var done = false
    val startPos = current.current.fold(_.pos, _.pos)

    while (!done) {
      current.current match {
        case Right(Token.RParen(endPos)) =>
          done = true
          current = advance(current)
          val meta = createMeta(startPos, endPos)
          return Right((Tuple(terms, meta), current))
        case Right(_) =>
          parseTupleItem(current) match {
            case Right((expr, exprState)) =>
              terms = terms :+ expr
              current = exprState
              current.current match {
                case Right(Token.Comma(_)) =>
                  current = advance(current)
                case Right(Token.RParen(endPos)) =>
                  done = true
                  current = advance(current)
                  val meta = createMeta(startPos, endPos)
                  return Right((Tuple(terms, meta), current))
                case _ =>
                  return Left(ParseError("Expected ',' or ')'", current.current.fold(_.pos, _.pos)))
              }
            case Left(err) => return Left(err)
          }
        case Left(err) => return Left(err)
      }
    }

    val endPos = current.current.fold(_.pos, _.pos)
    val meta = createMeta(startPos, endPos)
    Right((Tuple(terms, meta), current))
  }

  private def parseList(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    var terms = Vector.empty[Expr]
    var done = false
    val startPos = current.current.fold(_.pos, _.pos)

    while (!done) {
      current.current match {
        case Right(Token.RBracket(endPos)) =>
          done = true
          current = advance(current)
          val meta = createMeta(startPos, endPos)
          return Right((ListExpr(terms, meta), current))
        case Right(_) =>
          parseExprInternal(current) match {
            case Right((expr, exprState)) =>
              terms = terms :+ expr
              current = exprState
              current.current match {
                case Right(Token.Comma(_)) =>
                  current = advance(current)
                case Right(Token.RBracket(endPos)) =>
                  done = true
                  current = advance(current)
                  val meta = createMeta(startPos, endPos)
                  return Right((ListExpr(terms, meta), current))
                case _ =>
                  return Left(ParseError("Expected ',' or ']'", current.current.fold(_.pos, _.pos)))
              }
            case Left(err) => return Left(err)
          }
        case Left(err) => return Left(err)
      }
    }

    val endPos = current.current.fold(_.pos, _.pos)
    val meta = createMeta(startPos, endPos)
    Right((ListExpr(terms, meta), current))
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