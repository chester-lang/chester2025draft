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
  ObjectExprClauseOnValue, ObjectExprClause, ObjectClause, OpSeq, ErrorExpr, RecoverableParseError
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
    globalRecursionDepth += 1
    maxRecursionDepth = math.max(maxRecursionDepth, globalRecursionDepth)
    methodCallCounts = methodCallCounts.updated(name, methodCallCounts.getOrElse(name, 0) + 1)
    
    debug(s"ENTER $name (depth=$globalRecursionDepth, calls=${methodCallCounts(name)})")
    debugState(s"Before $name")
    
    try {
      val result = f
      debug(s"EXIT $name with result=$result")
      debugState(s"After $name")
      result
    } catch {
      case e: Throwable =>
        debug(s"ERROR in $name: ${e.getMessage}")
        e.printStackTrace()
        throw e
    } finally {
      globalRecursionDepth -= 1
    }
  }

  private def createMeta(startSourcePos: SourcePos, endSourcePos: SourcePos): Option[ExprMeta] = {
    debug(s"createMeta: start=$startSourcePos, end=$endSourcePos")
    if (ignoreLocation) {
      None
    } else {
      Some(ExprMeta(Some(startSourcePos.combine(endSourcePos)), None))
    }
  }

  private def getSourcePos(token: Either[ParseError, Token]): SourcePos = {
    debug(s"getSourcePos: token=$token")
    token match {
      case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
      case Right(t) => t.sourcePos
    }
  }

  private def getStartPos(token: Either[ParseError, Token]): Pos = 
    getSourcePos(token).range.start

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
  //    - No predefined keywords (if/then/else/val) or operators (+/-/*)
  //    - No special parsing rules for any identifiers
  //    - Semantic meaning determined in later passes
  //    - Examples:
  //      - Traditional: if x then y else z
  //      - Custom: myIf x myThen y myElse z
  //      - Both parse to: OpSeq([identifier, expr, identifier, expr, identifier, expr])
  // 
  // 2. Operator/Identifier Rules:
  //    - Operators start with operator symbols (.:=-+\|<>/?`~!@$%^&*)
  //    - Identifiers start with letters/emoji/underscore
  //    - Both can contain operator symbols and word symbols
  //    - See IdentifierRules.scala for complete rules
  // 
  // 3. Sequence Construction:
  //    - All terms form a uniform sequence: expr op expr op expr ...
  //    - Structure preserved for later semantic analysis
  //    - Examples:
  //      - 1 + 2 -> OpSeq([1, +, 2])
  //      - if x then y -> OpSeq([if, x, then, y])
  //      - val x = 1 -> OpSeq([val, x, =, 1])
  //      - myOp1 x myOp2 y -> OpSeq([myOp1, x, myOp2, y])
  // 
  // 4. Benefits:
  //    - Allows user-defined operators and keywords
  //    - Consistent parsing rules for all identifiers
  //    - Flexible operator definition and extension
  //    - Operator precedence and fixity handled in later passes
  //    - Supports domain-specific language extensions

  // Design Notes for Operator Sequence Parsing:
  // 
  // 1. Operators are handled uniformly through character-based identification
  // 2. No special casing of operators - determined by character patterns
  // 3. Operator sequences are built incrementally
  // 4. The sequence maintains the alternating pattern: term operator term operator ...
  // 5. Both prefix and infix operators are supported through the same mechanism
  def parseExpr(): Either[ParseError, (Expr, LexerState)] = withRecursion("parseExpr") {
    debug(s"parseExpr: state=$state")
    state.current match {
      case Right(Token.Operator(opChars, _)) =>
        val str = opChars.mkString
        if (strIsOperator(str)) {
          val op = Identifier(str, None)
          val afterOp = advance()
          afterOp.current match {
            case Right(Token.LParen(_)) =>
              parseTuple(afterOp).map { case (args, afterArgs) =>
                val telescope = args match {
                  case tuple: Tuple => tuple
                  case other => Tuple(Vector(other), None)
                }
                (FunctionCall(op, telescope, None), afterArgs)
              }
            case _ =>
              parseAtom(afterOp).map { case (rhs, afterRhs) =>
                (OpSeq(Vector(op, rhs), None), afterRhs)
              }
          }
        } else {
          parseAtom(state).flatMap { case (firstAtom, afterAtom) =>
            collectOpSeq(firstAtom, afterAtom)
          }
        }
      case _ =>
        parseAtom(state).flatMap { case (firstAtom, afterAtom) =>
          collectOpSeq(firstAtom, afterAtom)
        }
    }
  }

  /*
   * Collects a sequence of expressions and operators into an OpSeq
   * without considering operator precedence or fixity.
   * This allows for flexible operator definition and semantics in later passes.
   */
  def collectOpSeq(first: Expr, current: LexerState): Either[ParseError, (OpSeq, LexerState)] = {
    debug(s"collectOpSeq: first=$first, current=$current")
    var terms = Vector[Expr](first)
    var maxTerms = 100
    var done = false
    var state = current

    while (!done && maxTerms > 0) {
      maxTerms -= 1
      state.current match {
        case Right(Token.EOF(_)) | Right(Token.Comment(_, _)) | Right(Token.RParen(_)) | 
             Right(Token.RBrace(_)) | Right(Token.RBracket(_)) | Right(Token.Comma(_)) | Right(Token.Semicolon(_)) =>
          done = true

        case Right(token) => token match {
          case Token.Operator(opChars, _) => {
            val str = opChars.mkString
            if (strIsOperator(str)) {
              val op = Identifier(str, None)
              state = state.advance()
              parseAtom(state) match {
                case Left(err) => return Left(err)
                case Right((rhs, afterRhs)) => {
                  terms = terms :+ op :+ rhs
                  state = afterRhs
                }
              }
            } else {
              done = true
            }
          }

          case Token.Identifier(idChars, _) => {
            val str = idChars.mkString
            if (strIsOperator(str)) {
              val op = Identifier(str, None)
              state = state.advance()
              parseAtom(state) match {
                case Left(err) => return Left(err)
                case Right((rhs, afterRhs)) => {
                  terms = terms :+ op :+ rhs
                  state = afterRhs
                }
              }
            } else {
              done = true
            }
          }

          case _ =>
            done = true
        }

        case Left(err) =>
          return Left(err)
      }
    }

    if (maxTerms <= 0) {
      Left(ParseError("Too many terms in operator sequence", getStartPos(state.current)))
    } else {
      Right((OpSeq(terms, None), state))
    }
  }

  private def parseStmt(state: LexerState): Either[ParseError, (ExprStmt, LexerState)] = {
    debug("parseStmt: starting")
    if (state.index >= state.tokens.length) {
      debug("parseStmt: end of tokens")
      Left(ParseError("Unexpected end of input", getStartPos(state.current)))
    } else {
      parseExpr().map { case (expr, exprState: LexerState) =>
        debug(s"parseStmt: parsed expression: $expr")
        (ExprStmt(expr, None), exprState)
      }
    }
  }

  private def parseStmts(): Either[ParseError, List[Expr]] = {
    debug("parseStmts: starting")
    boundary {
      var stmts = List[Expr]()
      var current = peek()
      var stmtCount = 0
      val maxStmts = 50 // Reduced from 1000 to 50
      
      current.current match {
        case Right(Token.EOF(_)) => 
          debug("parseStmts: found EOF immediately")
          Right(stmts)
        case _ =>
          while (!current.current.exists(_.isInstanceOf[Token.EOF]) && stmtCount < maxStmts) {
            debug(s"parseStmts: processing statement #$stmtCount, current token=${current.current}")
            if (current.index >= current.tokens.length) {
              debug("parseStmts: reached end of tokens")
              break(Left(ParseError("Unexpected end of input", getStartPos(current.current))))
            }
            
            current.current match {
              case Left(_) | Right(_) if current.current.isLeft =>
                debug("parseStmts: error in current token")
                break(Left(ParseError("Expected ';' or EOF", getStartPos(current.current))))
              case Right(Token.Comment(_, _)) =>
                debug("parseStmts: skipping comment")
                current = advance()
              case Right(Token.Semicolon(_)) =>
                debug("parseStmts: skipping semicolon")
                current = advance()
              case _ =>
                parseStmt(current) match {
                  case Left(err) => 
                    debug(s"parseStmts: error parsing statement: $err")
                    break(Left(err))
                  case Right((stmt, newState)) =>
                    debug(s"parseStmts: successfully parsed statement: $stmt")
                    current = newState
                    stmts = stmts :+ stmt
                    stmtCount += 1
                }
            }
          }
          if (stmtCount >= maxStmts) {
            debug("parseStmts: statement limit exceeded!")
            Left(ParseError("Too many statements", getStartPos(current.current)))
          } else {
            debug(s"parseStmts: finished with $stmtCount statements")
            Right(stmts)
          }
      }
    }
  }

  /*
   * Parses atomic expressions including identifiers, literals, and parenthesized expressions.
   * Handles error recovery by attempting to parse partial expressions when possible.
   */
  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseAtom") {
    debug(s"parseAtom: state=$state")
    if (state.index >= state.tokens.length) {
      debug("parseAtom: end of tokens")
      Left(ParseError("Unexpected end of input", getStartPos(state.current)))
    } else state.current match {
      case Right(Token.LParen(sourcePos)) =>
        debug("parseAtom: found left paren, parsing tuple")
        parseTuple(state)
      case Right(Token.LBrace(sourcePos)) =>
        debug("parseAtom: found left brace, parsing block")
        parseBlock(state)
      case Right(Token.LBracket(sourcePos)) =>
        debug("parseAtom: found left bracket, parsing list")
        parseList(state)
      case Right(Token.IntegerLiteral(value, sourcePos)) =>
        debug(s"parseAtom: found integer literal: $value")
        val intValue = if (value.startsWith("0x") || value.startsWith("0X")) {
          BigInt(value.substring(2), 16)
        } else if (value.startsWith("0b") || value.startsWith("0B")) {
          BigInt(value.substring(2), 2)
        } else {
          BigInt(value)
        }
        Right((IntegerLiteral(intValue, None), advance()))
      case Right(Token.RationalLiteral(value, sourcePos)) =>
        debug(s"parseAtom: found rational literal: $value")
        Right((RationalLiteral(BigDecimal(value), None), advance()))
      case Right(Token.StringLiteral(chars, sourcePos)) =>
        debug(s"parseAtom: found string literal: ${chars.mkString}")
        Right((StringLiteral(chars.mkString, None), advance()))
      case Right(Token.Identifier(chars, sourcePos)) =>
        debug(s"parseAtom: found identifier: ${chars.mkString}")
        val id = chars.mkString
        val identifier = Identifier(id, None)
        val afterId = advance()
        afterId.current match {
          case Right(Token.LParen(_)) =>
            debug(s"parseAtom: found parentheses after identifier, creating function call")
            parseTuple(afterId).map { case (args, afterArgs) =>
              val telescope = args match {
                case tuple: Tuple => tuple
                case other => Tuple(Vector(other), None)
              }
              (FunctionCall(identifier, telescope, None), afterArgs)
            }
          case _ => Right((identifier, afterId))
        }
      case Right(token) =>
        debug(s"parseAtom: unexpected token: $token")
        Left(ParseError(s"Unexpected token: $token", getStartPos(state.current)))
      case Left(err) =>
        debug(s"parseAtom: error token: $err")
        Left(err)
    }
  }

  private def parseSuffix(state: LexerState, left: Expr): Either[ParseError, (Expr, LexerState)] = withRecursion("parseSuffix") {
    debug(s"parseSuffix: state=$state, left=$left")
    state.current match {
      case Right(Token.Operator(chars, sourcePos)) if chars.mkString == "->" =>
        debug("parseSuffix: found -> operator")
        val afterArrow = advance()
        parseAtom(afterArrow).map { case (rhs, rhsState) =>
          (OpSeq(Vector(left, Identifier("->", None), rhs), None), rhsState)
        }
      case _ => Right((left, state))
    }
  }

  /*
   * Parses blocks with error recovery support.
   * Attempts to parse partial blocks even when encountering errors.
   */
  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    state.current match {
      case Right(Token.LBrace(sourcePos)) => {
        val afterBrace = advance()
        afterBrace.current match {
          case Right(Token.RBrace(endSourcePos)) => {
            Right((Block(Vector.empty, None, createMeta(sourcePos, endSourcePos)), advance()))
          }
          case _ => {
            parseExpr().flatMap { case (expr, afterExpr) =>
              afterExpr.current match {
                case Right(Token.RBrace(endSourcePos)) => {
                  Right((Block(Vector.empty, Some(expr), createMeta(sourcePos, endSourcePos)), advance()))
                }
                case Right(Token.Semicolon(_)) => {
                  val afterSemi = advance()
                  parseExpr().flatMap { case (nextExpr, afterNext) =>
                    afterNext.current match {
                      case Right(Token.RBrace(endSourcePos)) => {
                        Right((Block(Vector(ExprStmt(expr, None)), Some(nextExpr), createMeta(sourcePos, endSourcePos)), advance()))
                      }
                      case Right(t) => Left(ParseError("Expected '}' after block expression", t.sourcePos.range.start))
                      case Left(err) => Left(err)
                    }
                  }
                }
                case Right(t) => Left(ParseError("Expected '}' or ';' after block expression", t.sourcePos.range.start))
                case Left(err) => Left(err)
              }
            }
          }
        }
      }
      case Right(t) => Left(ParseError("Expected '{' for block", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseTuple") {
    debug(s"parseTuple: state=$state")
    boundary {
      state.current match {
        case Right(Token.LParen(sourcePos)) => {
          debug("parseTuple: found opening parenthesis")
          var current = advance()
          var exprs = Vector[Expr]()
          var done = false

          while (!done) {
            current.current match {
              case Right(Token.RParen(_)) => {
                debug("parseTuple: found closing parenthesis")
                done = true
                current = advance()
              }
              case Right(Token.Comma(_)) => {
                debug("parseTuple: skipping comma")
                current = advance()
              }
              case _ => {
                debug("parseTuple: attempting to parse tuple item")
                parseExpr() match {
                  case Left(parseError) => {
                    debug(s"parseTuple: error parsing item: $parseError")
                    break(Left(parseError))
                  }
                  case Right((expr, afterExpr)) => {
                    debug(s"parseTuple: successfully parsed item: $expr")
                    exprs = exprs :+ expr
                    current = afterExpr
                  }
                }
              }
            }
          }
          Right((Tuple(exprs, None), current))
        }
        case Right(t) => Left(ParseError("Expected '('", t.sourcePos.range.start))
        case Left(err) => Left(err)
      }
    }
  }

  private def parseTupleItem(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    boundary {
      parseAtom(state).flatMap { case (expr, exprState) =>
        exprState.current match {
          case Right(Token.Operator(chars, sourcePos)) if chars.mkString == "->" =>
            val afterArrow = advance()
            parseAtom(afterArrow).map { case (rhs, rhsState) =>
              (OpSeq(Vector(expr, Identifier("->", None), rhs), None), rhsState)
            }
          case _ => Right((expr, exprState))
        }
      }
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseList") {
    debug(s"parseList: state=$state")
    boundary {
      state.current match {
        case Right(Token.LBracket(sourcePos)) =>
          debug("parseList: found opening bracket")
          var current = advance()
          var exprs = Vector[Expr]()
          var maxItems = 50 // Reduced from 1000 to 50

          while (maxItems > 0 && !current.current.exists(_.isInstanceOf[Token.RBracket])) {
            debug(s"parseList: processing item, maxItems=$maxItems, current=$current")
            if (current.index >= current.tokens.length) {
              debug("parseList: reached end of tokens")
              break(Right((RecoverableParseError(Some(ListExpr(exprs, None)), "Unexpected end of input while parsing list", getStartPos(state.current), None), current)))
            }

            current.current match {
              case Right(Token.Comment(_, _)) =>
                debug("parseList: skipping comment")
                current = advance()
              case Right(Token.Comma(_)) =>
                debug("parseList: skipping comma")
                current = advance()
              case Right(Token.RBracket(_)) =>
                debug("parseList: found closing bracket")
                break(Right((ListExpr(exprs, None), current)))
              case _ =>
                debug("parseList: attempting to parse list item")
                parseExpr() match {
                  case Left(parseError) =>
                    debug(s"parseList: error parsing item: $parseError")
                    break(Right((RecoverableParseError(Some(ListExpr(exprs, None)), parseError.message, parseError.pos, None), current)))
                  case Right((expr, afterExpr)) =>
                    debug(s"parseList: successfully parsed item: $expr")
                    exprs = exprs :+ expr
                    current = afterExpr
                    current.current match {
                      case Right(Token.RBracket(_)) =>
                        debug("parseList: found closing bracket")
                        break(Right((ListExpr(exprs, None), current)))
                    case Right(Token.Comma(_)) =>
                        debug("parseList: found comma separator")
                        current = advance()
                      case Right(t) =>
                        debug(s"parseList: unexpected token after item: $t")
                        break(Right((RecoverableParseError(Some(ListExpr(exprs, None)), "Expected ',' or ']' in list", t.sourcePos.range.start, None), current)))
                      case Left(parseError) =>
                        debug(s"parseList: error token after item: $parseError")
                        break(Right((RecoverableParseError(Some(ListExpr(exprs, None)), parseError.message, parseError.pos, None), current)))
                    }
                }
            }
            maxItems -= 1
          }

          if (maxItems <= 0) {
            debug("parseList: item limit exceeded!")
            Right((RecoverableParseError(Some(ListExpr(exprs, None)), "Too many items in list", getStartPos(state.current), None), current))
          } else {
            current.current match {
              case Right(Token.RBracket(endSourcePos)) =>
                debug("parseList: found closing bracket")
                Right((ListExpr(exprs, createMeta(sourcePos, endSourcePos)), advance()))
              case _ =>
                debug("parseList: missing closing bracket")
                Right((RecoverableParseError(Some(ListExpr(exprs, None)), "Expected ']' to close list", getStartPos(current.current), None), current))
            }
          }
        case Right(t) => 
          debug(s"parseList: missing opening bracket, found: $t")
          Right((RecoverableParseError(None, "Expected '[' for list", t.sourcePos.range.start, None), state))
        case Left(parseError) => 
          debug(s"parseList: error token: $parseError")
          Right((RecoverableParseError(None, parseError.message, parseError.pos, None), state))
      }
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    debug("parseExprList: starting")
    boundary {
      var current = state
      var exprs = Vector.empty[Expr]
      var done = false
      var exprCount = 0

      while (!done) {
        debug(s"parseExprList: processing expression #$exprCount, current token=${current.current}")
        current.current match {
          case Right(Token.EOF(_)) => {
            debug("parseExprList: found EOF")
            done = true
          }
          case Right(_) => {
            parseExpr() match {
              case Right((expr, exprState)) => {
                debug(s"parseExprList: parsed expression: $expr")
                exprs = exprs :+ expr
                current = exprState
                exprCount += 1
                current.current match {
                  case Right(Token.Semicolon(_)) => {
                    debug("parseExprList: found semicolon after expression")
                    current = advance()
                  }
                  case Right(Token.EOF(_)) => {
                    debug("parseExprList: found EOF after expression")
                    done = true
                  }
                  case Right(t) => {
                    debug(s"parseExprList: unexpected token after expression: $t")
                    break(Left(ParseError("Expected ';' or EOF", getStartPos(current.current))))
                  }
                  case Left(err) => {
                    debug(s"parseExprList: error token after expression: $err")
                    break(Left(err))
                  }
                }
              }
              case Left(err) => {
                debug(s"parseExprList: error parsing expression: $err")
                break(Left(err))
              }
            }
          }
          case Left(err) => {
            debug(s"parseExprList: error token: $err")
            break(Left(err))
          }
        }
      }

      debug(s"parseExprList: finished with $exprCount expressions")
      Right((exprs, current))
    }
  }

  private def parseObject(state: LexerState): Either[ParseError, (Expr, LexerState)] = withRecursion("parseObject") {
    debug(s"parseObject: state=$state")
    boundary {
      state.current match {
        case Right(Token.LBrace(sourcePos)) => {
          debug("parseObject: found opening brace")
          var current = advance()
          var clauses = Vector[ObjectClause]()
          var maxClauses = 50 // Reduced from 1000 to 50

          while (maxClauses > 0 && !current.current.exists(_.isInstanceOf[Token.RBrace])) {
            debug(s"parseObject: processing clause, maxClauses=$maxClauses, current=$current")
            if (current.index >= current.tokens.length) {
              debug("parseObject: reached end of tokens")
              break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Unexpected end of input while parsing object", getStartPos(state.current), None), current)))
            }

            current.current match {
              case Right(Token.Identifier(parts, idSourcePos)) => {
                debug(s"parseObject: found identifier '${parts.mkString}'")
                val key = Identifier(parts.mkString, createMeta(idSourcePos, idSourcePos))
                current = advance()

                current.current match {
                  case Right(Token.Colon(_)) | Right(Token.Equal(_)) => {
                    debug("parseObject: found colon or equals")
                    current = advance()
                    parseExpr() match {
                      case Left(parseError) => {
                        debug(s"parseObject: error parsing value: $parseError")
                        break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
                      }
                      case Right((value, afterValue)) => {
                        debug(s"parseObject: successfully parsed value: $value")
                        clauses = clauses :+ ObjectExprClauseOnValue(key, value)
                        current = afterValue
                      }
                    }
                  }
                  case Right(t) => {
                    debug(s"parseObject: unexpected token after key: $t")
                    break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected ':' or '=' after object field key", t.sourcePos.range.start, None), current)))
                  }
                  case Left(parseError) => {
                    debug(s"parseObject: error token after key: $parseError")
                    break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
                  }
                }
              }
              case Right(Token.Comment(_, _)) => {
                debug("parseObject: skipping comment")
                current = advance()
              }
              case Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
                debug("parseObject: skipping separator")
                current = advance()
              }
              case Right(t) => {
                debug(s"parseObject: unexpected token: $t")
                break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected identifier, '}', or comment", t.sourcePos.range.start, None), current)))
              }
              case Left(parseError) => {
                debug(s"parseObject: error token: $parseError")
                break(Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), parseError.message, parseError.pos, None), current)))
              }
            }
            maxClauses -= 1
          }

          if (maxClauses <= 0) {
            debug("parseObject: clause limit exceeded!")
            Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Too many clauses in object", getStartPos(state.current), None), current))
          } else {
            current.current match {
              case Right(Token.RBrace(endSourcePos)) => {
                debug("parseObject: found closing brace")
                Right((ObjectExpr(clauses, createMeta(sourcePos, endSourcePos)), advance()))
              }
              case _ => {
                debug("parseObject: missing closing brace")
                Right((RecoverableParseError(Some(ObjectExpr(clauses, None)), "Expected '}' to close object", getStartPos(current.current), None), current))
              }
            }
          }
        }
        case Right(t) => {
          debug(s"parseObject: missing opening brace, found: $t")
          Right((RecoverableParseError(None, "Expected '{' for object", t.sourcePos.range.start, None), state))
        }
        case Left(parseError) => {
          debug(s"parseObject: error token: $parseError")
          Right((RecoverableParseError(None, parseError.message, parseError.pos, None), state))
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
      parseExpr() match {
        case Left(err) => return Left(err)
        case Right((left, afterLeft)) => {
          if (afterLeft.current.exists(t => t match {
            case Token.Operator("->", _) => true
            case _ => false
          })) {
            val afterArrow = afterLeft.advance()
            parseExpr() match {
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
}