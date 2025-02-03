package chester.readerv2

/*
 * ReaderV2 Design Principles:
 * 
 * 1. Separation of Concerns:
 *    - Parser only produces OpSeq without knowledge of operator semantics
 *    - Operator precedence, fixity (infix/prefix/postfix/mixfix) handled in later passes
 *    - This separation allows flexible operator definition and extension
 * 
 * 2. Error Recovery:
 *    - Designed to handle incomplete/broken source code (e.g. during editing)
 *    - Attempts to produce meaningful partial results when possible
 *    - Uses ErrorExpr to represent recoverable parse errors inline
 *    - Continues parsing after encountering errors when safe to do so
 * 
 * 3. OpSeq Design:
 *    - Raw sequence of expressions and operators
 *    - No precedence or associativity information at parse time
 *    - Later passes will restructure based on operator properties
 *    - Allows for flexible operator definition and semantics
 * 
 * 4. Incremental Parsing:
 *    - Supports partial parsing of incomplete expressions
 *    - Maintains parser state for potential incremental updates
 *    - Useful for IDE integration and live editing
 */

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break
import chester.error.{Pos, SourcePos, RangeInFile}
import chester.error.WithPos
import chester.utils.WithUTF16
import io.github.iltotore.iron.autoRefine
import chester.reader.{ParseError, SourceOffset, ParserSource}
import chester.syntax.concrete.{
  Block, Expr, ExprMeta, ExprStmt, Identifier, ListExpr, ObjectExpr,
  QualifiedName, Tuple, FunctionCall, IntegerLiteral, RationalLiteral, StringLiteral,
  ObjectExprClauseOnValue, ObjectExprClause, ObjectClause, OpSeq, ErrorExpr, RecoverableParseError,
  MaybeTelescope
}
import chester.syntax.concrete.Literal.*
import spire.math.Rational
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator

case class StmtExpr(expr: Expr)

case class LexerState(
  tokens: Vector[Either[ParseError, Token]],
  index: Int
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = LexerState(tokens, index + 1)
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t) => t.sourcePos
  }
  override def toString: String = s"LexerState(index=$index, current=$current, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)
    
  var DEBUG = false // Global debug flag
  private var globalRecursionDepth = 0 // Track global recursion depth
  private var maxRecursionDepth = 0 // Track maximum recursion depth reached
  private var methodCallCounts = Map[String, Int]() // Track number of calls per method
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.{DEBUG, globalRecursionDepth, maxRecursionDepth, methodCallCounts}
  
  private def debug(msg: => String): Unit = if (DEBUG) {
    val indent = " " * globalRecursionDepth
    println(s"[DEBUG]$indent $msg")
    System.out.flush()
  }
  
  private def debugState(label: String): Unit = if (DEBUG) {
    debug(s"$label:")
    debug(s"  Current state: $state")
    debug(s"  Loop count: $loopCount")
    debug(s"  Recursion depth: $globalRecursionDepth")
    debug(s"  Max recursion depth: $maxRecursionDepth")
    debug(s"  Method call counts: ${methodCallCounts.mkString(", ")}")
    System.out.flush()
  }
  
  private var state: LexerState = LexerState(tokens.toVector, 0)
  private var loopCount = 0 // Track loop iterations
  
  private def withRecursion[T](name: String)(f: => T): T = {
    val MaxRecursionDepth = 100 // Add reasonable limit
    try {
      if (globalRecursionDepth >= MaxRecursionDepth) {
        throw new RuntimeException(s"Maximum recursion depth ($MaxRecursionDepth) exceeded")
      }
      globalRecursionDepth += 1
      maxRecursionDepth = math.max(maxRecursionDepth, globalRecursionDepth)
      methodCallCounts = methodCallCounts.updated(name, methodCallCounts.getOrElse(name, 0) + 1)
      
      debug(s"ENTER $name (depth=$globalRecursionDepth, calls=${methodCallCounts(name)})")
      debugState(s"Before $name")
      
      val result = f
      debug(s"EXIT $name with result=$result")
      debugState(s"After $name")
      result
    } catch {
      case e: Throwable =>
        debug(s"ERROR in $name: ${e.getMessage}")
        throw e
    } finally {
      globalRecursionDepth -= 1
    }
  }

  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] = {
    if (ignoreLocation) {
      None
    } else {
      (startPos, endPos) match {
        case (Some(start), Some(end)) =>
          Some(ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None))
        case (Some(pos), _) =>
          Some(ExprMeta(Some(pos), None))
        case (_, Some(pos)) =>
          Some(ExprMeta(Some(pos), None))
        case _ =>
          None
      }
    }
  }

  private def getSourcePos(token: Either[ParseError, Token]): SourcePos = {
    debug(s"getSourcePos: token=$token")
    token match {
      case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
      case Right(t) => t.sourcePos
    }
  }

  private def getStartPos(token: Either[ParseError, Token]): Pos = token match {
    case Right(t) => t.sourcePos.range.start
    case Left(err) => err.pos
  }

  private def peek(): LexerState = {
    debug(s"peek: current state=$state")
    state
  }

  private def advance(): LexerState = {
    val oldState = state
    state = LexerState(state.tokens, state.index + 1)
    debug(s"advance: from $oldState to $state")
    state
  }

  // Design Notes for Uniform Operator/Identifier Handling:
  // 
  // 1. No Special Cases:
  //    - All identifiers and operators are treated uniformly in parsing
  //    - IMPORTANT: No special cases for keywords like "if", "then", "else" - they are just identifiers
  //    - No predefined keywords (if/then/else/val) or operators (+/-/*)
  //    - No special parsing rules for any identifiers
  //    - Semantic meaning determined in later passes
  //    - Examples:
  //      - Traditional: if x then y else z
  //      - Custom: myIf x myThen y myElse z
  //      - Both parse to: OpSeq([identifier, expr, identifier, expr, identifier, expr])
  //      - Keywords like "if", "then", "else" are treated exactly the same as any other identifier
  // 
  // 2. Operator/Identifier Rules:
  //    - Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
  //    - Identifiers start with letters/emoji/underscore
  //    - Both can contain operator symbols and word symbols
  //    - See IdentifierRules.scala for complete rules
  //    - IMPORTANT: We use IdentifierRules.strIsOperator for uniform operator identification
  //    - NO local redefinition of operator rules to ensure consistency
  //    - Keywords are NOT special - they follow the same rules as any other identifier
  // 
  // 3. Sequence Construction:
  //    - All terms form a uniform sequence: expr op expr op expr ...
  //    - Structure preserved for later semantic analysis
  //    - Examples:
  //      - 1 + 2 -> OpSeq([1, +, 2])
  //      - if x then y -> OpSeq([if, x, then, y])  // "if" and "then" are just identifiers
  //      - val x = 1 -> OpSeq([val, x, =, 1])      // "val" is just an identifier
  //      - myOp1 x myOp2 y -> OpSeq([myOp1, x, myOp2, y])
  // 
  // 4. Benefits:
  //    - Allows user-defined operators and keywords
  //    - Consistent parsing rules for all identifiers
  //    - Flexible operator definition and extension
  //    - Operator precedence and fixity handled in later passes
  //    - Supports domain-specific language extensions
  //    - Single source of truth for operator identification (IdentifierRules)
  //    - No special cases means simpler, more maintainable code
  //    - Keywords can be redefined or extended by users

  // Design Notes for Operator Sequence Parsing:
  // 
  // 1. Operators are handled uniformly through character-based identification
  // 2. No special casing of operators - determined by character patterns in IdentifierRules
  // 3. Operator sequences are built incrementally
  // 4. The sequence maintains the alternating pattern: term operator term operator ...
  // 5. Both prefix and infix operators are supported through the same mechanism
  // 6. All operator identification is delegated to IdentifierRules.strIsOperator
  //    to maintain consistency across the codebase
  type LexerError = ParseError

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseExpr") {
    debug(s"parseExpr: state=$state")
    
    def buildOpSeq(terms: Vector[Expr], state: LexerState): Either[ParseError, (Expr, LexerState)] = {
      if (state.index >= state.tokens.length) {
        Right((if (terms.length == 1) terms.head else OpSeq(terms, None), state))
      } else {
        state.current match {
          case Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) | Right(Token.Comma(_)) | Right(Token.EOF(_)) =>
            Right((if (terms.length == 1) terms.head else OpSeq(terms, None), state))
          case Right(Token.Operator(chars, _)) =>
            val op = Identifier(chars, None)
            val afterOp = advance()
            parseAtom(afterOp).flatMap { case (nextTerm, afterTerm) =>
              buildOpSeq(terms :+ op :+ nextTerm, afterTerm)
            }
          case Right(Token.Identifier(chars, _)) =>
            val id = Identifier(chars.map(_.text).mkString, None)
            val afterId = advance()
            parseAtom(afterId).flatMap { case (nextTerm, afterTerm) =>
              buildOpSeq(terms :+ id :+ nextTerm, afterTerm)
            }
          case _ =>
            parseAtom(state).flatMap { case (nextTerm, afterTerm) =>
              buildOpSeq(terms :+ nextTerm, afterTerm)
            }
        }
      }
    }

    parseAtom(state).flatMap { case (firstTerm, afterFirst) =>
      buildOpSeq(Vector(firstTerm), afterFirst)
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    var current = state
    var exprs = Vector.empty[Expr]
    var maxExprs = 50

    while (maxExprs > 0 && !current.current.exists(_.isInstanceOf[Token.RParen])) {
      if (current.index >= current.tokens.length) {
        return Left(ParseError("Unexpected end of input while parsing expressions", getStartPos(state.current)))
      }

      current.current match {
        case Right(Token.Comma(_)) =>
          current = advance()
        case Right(Token.RParen(_)) =>
          current = advance()
        case _ =>
          parseExpr(current) match {
            case Left(err) => return Left(err)
            case Right((expr, afterExpr)) =>
              exprs = exprs :+ expr
              current = afterExpr
              current.current match {
                case Right(Token.RParen(_)) =>
                  return Right((exprs, current))
                case Right(Token.Comma(_)) =>
                  current = advance()
                case Right(t) =>
                  return Left(ParseError("Expected ',' or ')' after expression", t.sourcePos.range.start))
                case Left(err) => return Left(err)
              }
          }
      }
      maxExprs -= 1
    }

    if (maxExprs <= 0) {
      Left(ParseError("Too many expressions", getStartPos(state.current)))
    } else {
      Right((exprs, current))
    }
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseAtom") {
    debug(s"parseAtom: state=$state")
    if (state.index >= state.tokens.length) {
      debug("parseAtom: end of tokens")
      Left(ParseError("Unexpected end of input", getStartPos(state.current)))
    } else {
      state.current match {
        case Right(Token.IntegerLiteral(value, sourcePos)) =>
          Right((IntegerLiteral(BigInt(value), createMeta(Some(sourcePos), Some(sourcePos))), advance()))
        case Right(Token.RationalLiteral(value, sourcePos)) =>
          Right((RationalLiteral(spire.math.Rational(BigDecimal(value)), createMeta(Some(sourcePos), Some(sourcePos))), advance()))
        case Right(Token.StringLiteral(value, sourcePos)) =>
          Right((StringLiteral(value.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), advance()))
        case Right(Token.Identifier(value, sourcePos)) =>
          val afterId = advance()
          afterId.current match {
            case Right(Token.LParen(_)) =>
              parseFunctionCall(afterId, Identifier(value.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))))
            case _ =>
              Right((Identifier(value.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), afterId))
          }
        case Right(Token.LParen(_)) =>
          parseTuple(state)
        case Right(Token.LBrace(_)) =>
          parseBlock(state)
        case Right(Token.LBracket(_)) =>
          parseList(state)
        case Right(Token.Operator(value, sourcePos)) =>
          // Handle prefix operators
          val afterOp = advance()
          parseAtom(afterOp).map { case (expr, afterExpr) =>
            (OpSeq(Vector(Identifier(value, createMeta(Some(sourcePos), Some(sourcePos))), expr), None), afterExpr)
          }
        case _ =>
          Left(ParseError("Unexpected token", getStartPos(state.current)))
      }
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (ListExpr, LexerState)] = {
    state.current match {
      case Right(Token.LBracket(sourcePos)) => {
        var current = advance()
        var exprs = Vector.empty[Expr]
        var maxExprs = 50

        while (maxExprs > 0 && !current.current.exists(_.isInstanceOf[Token.RBracket])) {
          if (current.index >= current.tokens.length) {
            return Left(ParseError("Unexpected end of input while parsing list", getStartPos(state.current)))
          }

          current.current match {
            case Right(Token.Comma(_)) =>
              current = advance()
            case Right(Token.RBracket(_)) =>
              current = advance()
            case _ =>
              parseExpr(current) match {
                case Left(err) => return Left(err)
                case Right((expr, afterExpr)) =>
                  exprs = exprs :+ expr
                  current = afterExpr
                  current.current match {
                    case Right(Token.RBracket(endPos)) =>
                      return Right((ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))), advance()))
                    case Right(Token.Comma(_)) =>
                      current = advance()
                    case Right(t) =>
                      return Left(ParseError("Expected ',' or ']' in list", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
              }
          }
          maxExprs -= 1
        }

        if (maxExprs <= 0) {
          Left(ParseError("Too many expressions in list", getStartPos(state.current)))
        } else {
          current.current match {
            case Right(Token.RBracket(endPos)) =>
              Right((ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))), advance()))
            case _ =>
              Left(ParseError("Expected ']' to close list", getStartPos(current.current)))
          }
        }
      }
      case _ =>
        Left(ParseError("Expected '[' for list", getStartPos(state.current)))
    }
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(sourcePos)) => {
        var current = advance()
        var exprs = Vector.empty[Expr]
        var maxExprs = 50

        while (maxExprs > 0 && !current.current.exists(_.isInstanceOf[Token.RBrace])) {
          if (current.index >= current.tokens.length) {
            return Left(ParseError("Unexpected end of input while parsing block", getStartPos(state.current)))
          }

          current.current match {
            case Right(Token.Semicolon(_)) =>
              current = advance()
            case Right(Token.RBrace(_)) =>
              current = advance()
            case _ =>
              parseExpr(current) match {
                case Left(err) => return Left(err)
                case Right((expr, afterExpr)) =>
                  exprs = exprs :+ expr
                  current = afterExpr
                  current.current match {
                    case Right(Token.RBrace(endPos)) =>
                      current = advance()
                      return Right((Block(exprs, None, createMeta(Some(sourcePos), Some(endPos))), current))
                    case Right(Token.Semicolon(_)) =>
                      current = advance()
                    case Right(t) => 
                      // Allow expressions without semicolons
                      ()
                    case Left(err) => return Left(err)
                  }
              }
          }
          maxExprs -= 1
        }

        if (maxExprs <= 0) {
          Left(ParseError("Too many expressions in block", getStartPos(state.current)))
        } else {
          current.current match {
            case Right(Token.RBrace(endPos)) =>
              Right((Block(exprs, None, createMeta(Some(sourcePos), Some(endPos))), advance()))
            case _ =>
              Left(ParseError("Expected '}' to close block", getStartPos(current.current)))
          }
        }
      }
      case _ =>
        Left(ParseError("Expected '{' for block", getStartPos(state.current)))
    }
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Tuple, LexerState)] = {
    state.current match {
      case Right(Token.LParen(sourcePos)) =>
        var current = advance()
        var exprs = Vector.empty[Expr]
        var maxExprs = 50

        while (maxExprs > 0 && !current.current.exists(_.isInstanceOf[Token.RParen])) {
          if (current.index >= current.tokens.length) {
            return Left(ParseError("Unexpected end of input while parsing tuple", getStartPos(state.current)))
          }

          current.current match {
            case Right(Token.Comma(_)) =>
              current = advance()
            case Right(Token.RParen(_)) =>
              current = advance()
            case _ =>
              parseExpr(current) match {
                case Left(err) => return Left(err)
                case Right((expr, afterExpr)) =>
                  exprs = exprs :+ expr
                  current = afterExpr
                  current.current match {
                    case Right(Token.RParen(endPos)) =>
                      return Right((Tuple(exprs, createMeta(Some(sourcePos), Some(endPos))), advance()))
                    case Right(Token.Comma(_)) =>
                      current = advance()
                    case Right(t) =>
                      return Left(ParseError("Expected ',' or ')' in tuple", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
              }
          }
          maxExprs -= 1
        }

        if (maxExprs <= 0) {
          Left(ParseError("Too many expressions in tuple", getStartPos(state.current)))
        } else {
          current.current match {
            case Right(Token.RParen(endPos)) =>
              Right((Tuple(exprs, createMeta(Some(sourcePos), Some(endPos))), advance()))
            case _ =>
              Left(ParseError("Expected ')' to close tuple", getStartPos(current.current)))
          }
        }
      case _ =>
        Left(ParseError("Expected '(' for tuple", getStartPos(state.current)))
    }
  }

  private def parseFunctionCall(state: LexerState, function: Expr): Either[ParseError, (FunctionCall, LexerState)] = {
    state.current match {
      case Right(Token.LParen(sourcePos)) =>
        parseTuple(state) match {
          case Right((args, afterArgs)) =>
            Right((FunctionCall(function, args, createMeta(Some(sourcePos), Some(afterArgs.sourcePos))), afterArgs))
          case Left(err) => Left(err)
        }
      case _ =>
        Left(ParseError("Expected '(' for function call", getStartPos(state.current)))
    }
  }

  private def parseSuffix(state: LexerState, left: Expr): Either[ParseError, (Expr, LexerState)] = withRecursion("parseSuffix") {
    debug(s"parseSuffix: state=$state, left=$left")
    state.current match {
      case Right(Token.Operator(chars, sourcePos)) if chars.mkString == "->" => {
        debug("parseSuffix: found -> operator")
        val afterArrow = advance()
        parseAtom(afterArrow).map { case (rhs, rhsState) => {
          (OpSeq(Vector(left, Identifier("->", None), rhs), None), rhsState)
        }}
      }
      case _ => Right((left, state))
    }
  }

  private def parseTupleItem(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    parseExpr(state)
  }

  private def parseObject(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseObject") {
    debug(s"parseObject: state=$state")
    boundary {
      var current = state
      var clauses = Vector.empty[ObjectExprClauseOnValue]
      var maxClauses = 50

      while (maxClauses > 0) {
        debug(s"parseObject: processing clause, maxClauses=$maxClauses")
        if (current.index >= current.tokens.length) {
          debug("parseObject: reached end of tokens")
          break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Unexpected end of input", getStartPos(state.current), None), current)))
        }

          current.current match {
          case Right(Token.Comment(_, _)) => {
            debug("parseObject: skipping comment")
            current = advance()
          }
          case Right(Token.Comma(_)) => {
            debug("parseObject: skipping comma")
            current = advance()
          }
          case Right(Token.RBrace(_)) => {
            debug("parseObject: found closing brace")
            break(Right((ObjectExpr(clauses, None), current)))
          }
          case _ => {
            debug("parseObject: attempting to parse key")
            parseExpr(current) match {
              case Left(parseError) => {
                debug(s"parseObject: error parsing key: $parseError")
                break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
              }
              case Right((key, afterKey)) => {
                debug(s"parseObject: successfully parsed key: $key")
                afterKey.current match {
                  case Right(Token.Operator("->", _)) => {
                    val afterArrow = afterKey.advance()
                    debug("parseObject: found arrow, parsing value")
                    parseExpr(afterArrow) match {
                      case Left(parseError) => {
                        debug(s"parseObject: error parsing value: $parseError")
                        break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
                      }
                      case Right((value, afterValue)) => {
                        debug(s"parseObject: successfully parsed value: $value")
                          clauses = clauses :+ ObjectExprClauseOnValue(key, value)
                        current = afterValue
                        current.current match {
                          case Right(Token.RBrace(_)) => {
                            debug("parseObject: found closing brace")
                            break(Right((ObjectExpr(clauses, None), current)))
                          }
                          case Right(Token.Comma(_)) => {
                            debug("parseObject: found comma separator")
                            current = advance()
                          }
                          case Right(t) => {
                            debug(s"parseObject: unexpected token after value: $t")
                            break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected ',' or '}' in object", t.sourcePos.range.start, None), current)))
                          }
                          case Left(parseError) => {
                            debug(s"parseObject: error token after value: $parseError")
                            break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
                          }
                        }
                      }
                    }
                  }
                  case Right(t) => {
                    debug(s"parseObject: missing arrow after key: $t")
                    break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected '->' after key in object", t.sourcePos.range.start, None), current)))
                  }
                  case Left(parseError) => {
                    debug(s"parseObject: error token after key: $parseError")
                    break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
                  }
                }
              }
            }
          }
        }
        maxClauses -= 1
      }

      if (maxClauses <= 0) {
        debug("parseObject: clause limit exceeded!")
        Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Too many clauses in object", getStartPos(state.current), None), current))
      } else {
        current.current match {
          case Right(Token.RBrace(_)) => {
            debug("parseObject: found closing brace")
            Right((ObjectExpr(clauses, None), current))
          }
          case _ => {
            debug("parseObject: missing closing brace")
            Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected '}' to close object", getStartPos(current.current), None), current))
          }
        }
      }
    }
  }

  private def parseTelescope(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      state.current match {
        case Right(Token.LParen(sourcePos)) => {
          val afterParen = advance()
          afterParen.current match {
            case Right(Token.RParen(_)) => {
              Right((Tuple(Vector.empty, None), advance()))
            }
            case _ => {
              parseTupleItem(afterParen).flatMap { case (first, firstState) => {
                var exprs = Vector(first)
                var currentState = firstState
                var done = false

                while (!done) {
                  currentState.current match {
                    case Right(Token.RParen(_)) => {
                      done = true
                      currentState = advance()
                    }
                    case Right(Token.Comma(_)) => {
                      currentState = advance()
                      parseTupleItem(currentState) match {
                        case Right((expr, exprState)) => {
                          exprs = exprs :+ expr
                          currentState = exprState
                        }
                        case Left(err) => break(Left(err))
                      }
                    }
                    case Right(t) => break(Left(ParseError("Expected ',' or ')'", t.sourcePos.range.start)))
                    case Left(err) => break(Left(err))
                  }
                }
                Right((Tuple(exprs, None), currentState))
              }}
            }
          }
        }
        case Right(t) => Left(ParseError("Expected '('", t.sourcePos.range.start))
      case Left(err) => Left(err)
      }
    }
  }

  def parseObjectExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseObjectExpr") {
    var clauses = Vector.empty[ObjectExprClauseOnValue]
    var currentState = state
    
    while (true) {
      parseExpr(currentState) match {
        case Left(err) => return Left(err)
        case Right((left, afterLeft)) => {
          if (afterLeft.current.exists(t => t match {
            case Token.Operator("->", _) => true
            case _ => false
          })) {
            val afterArrow = afterLeft.advance()
            parseExpr(afterArrow) match {
              case Left(err) => return Left(err)
              case Right((right, afterRight)) =>
                clauses = clauses :+ ObjectExprClauseOnValue(left, right)
                if (afterRight.current.exists(t => t.isInstanceOf[Token.Comma])) {
                  currentState = afterRight.advance()
                } else {
                  return Right((ObjectExpr(clauses, None), afterRight))
                }
            }
          } else {
            return Left(ParseError("Expected '->' in object expression", afterLeft.sourcePos.range.start))
          }
        }
      }
    }
    
    Right((ObjectExpr(clauses, None), currentState))
  }

  def collectIdentifier(state: LexerState): (Vector[StringChar], LexerState) = {
    var chars = Vector.empty[StringChar]
    var currentState = state
    
    while (!currentState.isAtEnd && currentState.current.exists(token => token.isInstanceOf[Token.Identifier])) {
      currentState.current match {
        case Right(id: Token.Identifier) =>
          chars = chars ++ id.parts
          currentState = currentState.advance()
        case _ => throw new RuntimeException("Unreachable: exists check ensures this case never happens")
      }
    }
    
    (chars, currentState)
  }

  def isIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(token) => token.isInstanceOf[Token.Identifier]
    case _ => false
  }

  def expectIdentifier(expected: String, state: LexerState): Either[ParseError, LexerState] = {
    state.current match {
      case Right(Token.Identifier(chars, _)) if chars.map(_.text).mkString == expected =>
        Right(advance())
      case other =>
        Left(ParseError(s"Expected identifier '$expected' but got $other", state.sourcePos.range.start))
    }
  }
}