package chester.readerv2

/*
 * ReaderV2 implements Chester's token-based parser with unified symbol treatment.
 * 
 * For detailed documentation on the parser design principles, syntax decisions,
 * and implementation approach, see:
 *   docs/src/dev/parser-migration.md
 *
 * Key principles:
 * - Uniform symbol treatment (no special keywords)
 * - Space and newline significance in specific contexts
 * - Block return value semantics
 * - Operator sequences with later precedence resolution
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
  Block,
  Expr,
  ExprMeta,
  ExprStmt,
  ListExpr,
  ObjectExpr,
  QualifiedName,
  Tuple,
  FunctionCall,
  OpSeq,
  ErrorExpr,
  RecoverableParseError,
  MaybeTelescope,
  DotCall,
  ObjectExprClauseOnValue,
  ObjectExprClause,
  ObjectClause
}
import chester.syntax.concrete.{
  Identifier => ConcreteIdentifier,
  IntegerLiteral => ConcreteIntegerLiteral,
  RationalLiteral => ConcreteRationalLiteral,
  StringLiteral => ConcreteStringLiteral
}
import chester.syntax.concrete.Literal.*
import spire.math.Rational
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import scala.reflect.ClassTag
import chester.error.*
import chester.reader.*
import chester.syntax.*
import chester.syntax.concrete.*
import chester.utils.*
import Token.*

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
    case Right(t)  => t.sourcePos
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
      case Right(t)  => t.sourcePos
    }
  }

  private def getStartPos(token: Either[ParseError, Token]): Pos = token match {
    case Right(t)  => t.sourcePos.range.start
    case Left(err) => err.pos
  }

  private def peek(): LexerState = {
    debug(s"peek: current state=$state")
    state
  }

  private def advance(): LexerState = {
    if (state.index + 1 >= state.tokens.length) {
      LexerState(state.tokens, state.index + 1)
    } else {
      LexerState(state.tokens, state.index + 1)
    }
  }

  // See docs/src/dev/parser-migration.md for parser design documentation
  type LexerError = ParseError

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    var current = skipComments(state)
    var terms = Vector.empty[Expr]
    debug(s"Starting parseExpr with state: $current")

    def buildOpSeq(terms: Vector[Expr]): Either[ParseError, Expr] = {
      debug(s"Building OpSeq with terms: $terms")
      if (terms.isEmpty) {
        Left(ParseError("Empty operator sequence", getStartPos(state.current)))
      } else if (terms.length == 1) {
        Right(terms.head)
      } else {
        Right(OpSeq(terms, None))
      }
    }

    def parseRest(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
      // Create local terms vector starting with the input expression
      var localTerms = Vector(expr)
      
      debug(s"parseRest called with expr: $expr, state: $state, current terms: $localTerms")
      val current = skipComments(state)
      current.current match {
        case Right(Token.EOF(_)) | Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) | Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
          debug("parseRest: Hit terminator token")
          buildOpSeq(localTerms).map(result => (result, current))
        }
        case Right(Token.LBrace(braceSourcePos)) => {
          debug("parseRest: Found LBrace after expression, treating as block argument")
          // The LBrace is a block argument to the previous expression
          parseBlock(current).flatMap { case (block, afterBlock) =>
            // Handle block argument differently based on previous expression type
            val newExpr = expr match {
              // If previous expression is already a function call, add the block as another argument
              case funcCall: FunctionCall => {
                debug("parseRest: Adding block as argument to existing function call")
                // Create a nested function call where the previous function call becomes the function
                // and the block becomes the argument
                FunctionCall(
                  funcCall,
                  Tuple(Vector(block), createMeta(None, None)),
                  createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
                )
              }
              // If previous expression is an identifier, create a function call with the block as argument
              case id: ConcreteIdentifier => {
                debug("parseRest: Creating function call with block argument from identifier")
                FunctionCall(
                  id,
                  Tuple(Vector(block), createMeta(None, None)),
                  createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
                )
              }
              // Otherwise, just add the block as a term in the OpSeq
              case _ => {
                debug("parseRest: Default handling for block after expression")
                block
              }
            }
            
            // For function calls with blocks, don't wrap in OpSeq
            if (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[chester.syntax.concrete.Identifier] || 
                newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[FunctionCall]) {
              debug("parseRest: Returning function call with block directly")
              
              // Check for additional tokens after the block
              if (afterBlock.current.exists(t => t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] || 
                  t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] || 
                  t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon])) {
                // No more tokens, return the function call directly
                Right((newExpr, afterBlock))
              } else {
                // More tokens follow, continue parsing
                parseRest(newExpr, afterBlock)
              }
            } else {
              // Replace the last term with the new expression that includes the block
              localTerms = localTerms.dropRight(1) :+ newExpr
              debug(s"parseRest: After handling block argument, terms: $localTerms")
              parseRest(newExpr, afterBlock).map { case (result, finalState) =>
                // Handle the case where parseRest returns another OpSeq
                result match {
                  case opSeq: OpSeq =>
                    // If it's already an OpSeq, we need to merge with our current terms
                    val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                    (OpSeq(mergedTerms, None), finalState)
                  case _ =>
                    // If not an OpSeq, just return our current terms as an OpSeq
                    (OpSeq(localTerms, None), finalState)
                }
              }
            }
          }
        }
        case Right(Token.Colon(sourcePos)) => {
          debug("parseRest: Found colon")
          val afterColon = current.advance()
          localTerms = localTerms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
          debug(s"parseRest: After adding colon, terms: $localTerms")
          parseAtom(afterColon).flatMap { case (next, afterNext) =>
            localTerms = localTerms :+ next
            debug(s"parseRest: After parsing atom after colon, terms: $localTerms")
            parseRest(next, afterNext).map { case (result, finalState) =>
              // Handle the case where parseRest returns another OpSeq
              result match {
                case opSeq: OpSeq =>
                  // If it's already an OpSeq, we need to merge with our current terms
                  val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                  (OpSeq(mergedTerms, None), finalState)
                case _ =>
                  // If not an OpSeq, just return our current terms as an OpSeq
                  (OpSeq(localTerms, None), finalState)
              }
            }
          }
        }
        case Right(Token.Dot(dotSourcePos)) => {
          debug("parseRest: Found dot")
          handleDotCall(dotSourcePos, current, localTerms).flatMap { case (dotCall, newState) =>
            localTerms = Vector(dotCall)
            debug(s"parseRest: After dot call, terms: $localTerms")
            parseRest(dotCall, newState)
          }
        }
        case Right(Token.Operator(op, sourcePos)) => {
          debug(s"parseRest: Found operator $op")
          val afterOp = current.advance()
          // Check if this is a terminating operator (like * in vararg context)
          if (op == "*" && isVarargContext(current)) {
            localTerms = localTerms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))
            debug(s"parseRest: Added terminating operator *, terms: $localTerms")
            buildOpSeq(localTerms).map(result => (result, afterOp))
          } else {
            // Add operator to terms
            localTerms = localTerms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))
            parseAtom(afterOp).flatMap { case (next, afterNext) =>
              debug(s"parseRest: After parsing atom after operator, got: $next")
              localTerms = localTerms :+ next
              debug(s"parseRest: Updated terms after operator: $localTerms")
              // Continue parsing the rest
              parseRest(next, afterNext).map { case (result, finalState) =>
                // If the result is an OpSeq, merge with our current terms
                result match {
                  case opSeq: OpSeq =>
                    // Remove the last term (which was passed to recursive call) to avoid duplication
                    val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                    (OpSeq(mergedTerms, None), finalState)
                  case _ =>
                    // If not an OpSeq, just return our current terms as an OpSeq
                    (OpSeq(localTerms, None), finalState)
                }
              }
            }
          }
        }
        case Right(Token.Identifier(chars, sourcePos)) => {
          val text = chars.map(_.text).mkString
          debug(s"parseRest: Found identifier $text")
          val afterId = current.advance()
          afterId.current match {
            case Right(Token.LParen(_)) => {
              debug("parseRest: Found lparen after identifier")
              parseTuple(afterId).flatMap { case (tuple, afterTuple) =>
                val functionCall = FunctionCall(
                  ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                )
                localTerms = localTerms :+ functionCall
                debug(s"parseRest: After function call, terms: $localTerms")
                parseRest(functionCall, afterTuple).map { case (result, finalState) =>
                  // Handle the case where parseRest returns another OpSeq
                  result match {
                    case opSeq: OpSeq =>
                      // If it's already an OpSeq, we need to merge with our current terms
                      val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                      (OpSeq(mergedTerms, None), finalState)
                    case _ =>
                      // If not an OpSeq, just return our current terms as an OpSeq
                      (OpSeq(localTerms, None), finalState)
                  }
                }
              }
            }
            case Right(Token.LBrace(_)) => {
              debug("parseRest: Found lbrace after identifier")
              parseBlock(afterId).flatMap { case (block, afterBlock) =>
                // Create a FunctionCall with the block as a tuple argument
                val blockFunctionCall = FunctionCall(
                  ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
                  Tuple(Vector(block), createMeta(None, None)),
                  createMeta(Some(sourcePos), Some(sourcePos))
                )
                localTerms = localTerms :+ blockFunctionCall
                debug(s"parseRest: After block function call, terms: $localTerms")
                parseRest(blockFunctionCall, afterBlock).map { case (result, finalState) =>
                  // Handle the case where parseRest returns another OpSeq
                  result match {
                    case opSeq: OpSeq =>
                      // If it's already an OpSeq, we need to merge with our current terms
                      val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq 
                      (OpSeq(mergedTerms, None), finalState)
                    case _ =>
                      // If not an OpSeq, just return our current terms as an OpSeq
                      (OpSeq(localTerms, None), finalState)
                  }
                }
              }
            }
            case Right(Token.Operator(op, opSourcePos)) => {
              debug(s"parseRest: Found operator $op after identifier")
              val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
              localTerms = localTerms :+ id
              val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
              localTerms = localTerms :+ opId
              debug(s"parseRest: After adding id and op, terms: $localTerms")
              val afterOp = afterId.advance()
              parseAtom(afterOp).flatMap { case (next, afterNext) =>
                localTerms = localTerms :+ next
                debug(s"parseRest: After parsing atom after operator, terms: $localTerms")
                parseRest(next, afterNext).map { case (result, finalState) =>
                  // Handle the case where parseRest returns another OpSeq
                  result match {
                    case opSeq: OpSeq =>
                      // If it's already an OpSeq, we need to merge with our current terms
                      val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                      (OpSeq(mergedTerms, None), finalState)
                    case _ =>
                      // If not an OpSeq, just return our current terms as an OpSeq
                      (OpSeq(localTerms, None), finalState)
                  }
                }
              }
            }
            case _ => {
              debug(s"parseRest: Found bare identifier $text")
              val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
              localTerms = localTerms :+ id
              debug(s"parseRest: After adding bare id, terms: $localTerms")
              parseRest(id, afterId).map { case (result, finalState) =>
                // Handle the case where parseRest returns another OpSeq
                result match {
                  case opSeq: OpSeq =>
                    // If it's already an OpSeq, we need to merge with our current terms
                    val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                    (OpSeq(mergedTerms, None), finalState)
                  case _ =>
                    // If not an OpSeq, just return our current terms as an OpSeq
                    (OpSeq(localTerms, None), finalState)
                }
              }
            }
          }
        }
        case Right(_) => {
          debug("parseRest: Found other token, parsing as atom")
          parseAtom(current).flatMap { case (next, afterNext) =>
            localTerms = localTerms :+ next
            debug(s"parseRest: After parsing other token as atom, terms: $localTerms")
            parseRest(next, afterNext).map { case (result, finalState) =>
              // Handle the case where parseRest returns another OpSeq
              result match {
                case opSeq: OpSeq =>
                  // If it's already an OpSeq, we need to merge with our current terms
                  val mergedTerms = localTerms.dropRight(1) ++ opSeq.seq
                  (OpSeq(mergedTerms, None), finalState)
                case _ =>
                  // If not an OpSeq, just return our current terms as an OpSeq
                  (OpSeq(localTerms, None), finalState)
              }
            }
          }
        }
        case Left(error) => {
          debug(s"parseRest: Got error: $error")
          Left(error)
        }
      }
    }

    current.current match {
      case Right(Token.Operator(op, sourcePos)) => {
        debug(s"parseExpr: Starting with operator $op")
        val afterOp = current.advance()
        afterOp.current match {
          case Right(Token.LParen(_)) => {
            debug("parseExpr: Found lparen after initial operator")
            parseTuple(afterOp).map { case (tuple, afterTuple) =>
              (
                FunctionCall(
                  ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                ),
                afterTuple
              )
            }
          }
          case _ => {
            debug("parseExpr: Parsing atom after initial operator")
            // Create an operator term
            terms = Vector(ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))))
            parseAtom(afterOp).flatMap { case (expr, afterExpr) =>
              terms = terms :+ expr
              debug(s"parseExpr: After initial operator and atom, terms: $terms")
              // For prefix operators, we should wrap terms in an OpSeq
              if (afterExpr.current.exists(t => t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] || 
                  t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] || 
                  t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon])) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right((OpSeq(terms, None), afterExpr))
              } else {
                parseRest(expr, afterExpr)
              }
            }
          }
        }
      }
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(chars.map(_.text).mkString) => {
        // Handle keyword operators like "not", "if", etc. as prefix operators
        debug(s"parseExpr: Starting with keyword operator ${chars.map(_.text).mkString}")
        val afterOp = current.advance()
        // Create an operator term
        terms = Vector(ConcreteIdentifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))))
        parseAtom(afterOp).flatMap { case (expr, afterExpr) =>
          terms = terms :+ expr
          debug(s"parseExpr: After initial keyword operator and atom, terms: $terms")
          // For prefix operators, we should wrap terms in an OpSeq immediately if at EOF or delimiter
          if (afterExpr.current.exists(t => t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] || 
              t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] || 
              t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon])) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            Right((OpSeq(terms, None), afterExpr))
          } else {
            parseRest(expr, afterExpr)
          }
        }
      }
      case _ => {
        debug("parseExpr: Starting with atom")
        parseAtom(current).flatMap { case (first, afterFirst) =>
          debug(s"parseExpr: After initial atom, got: $first")
          parseRest(first, afterFirst)
        }
      }
    }
  }

  def handleDotCall(dotSourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
    val afterDot = state.advance() // Skip the dot
    afterDot.current match {
      case Right(Token.Identifier(chars1, idSourcePos1)) => {
        val afterId = afterDot.advance()
        val field = ConcreteIdentifier(chars1.map(_.text).mkString, createMeta(Some(idSourcePos1), Some(idSourcePos1)))
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
          state.current match {
            case Right(Token.LParen(_)) => {
              parseTuple(state).flatMap { case (args, afterArgs) =>
                telescope = telescope :+ args
                parseNextTelescope(afterArgs)
              }
            }
            case Right(Token.LBrace(_)) => {
              parseBlock(state).flatMap { case (block, afterBlock) =>
                telescope = telescope :+ Tuple(Vector(block), None)
                parseNextTelescope(afterBlock)
              }
            }
            case Right(Token.Dot(nextDotSourcePos)) => {
              val dotCall = DotCall(terms.last, field, telescope, createMeta(Some(dotSourcePos), Some(state.sourcePos)))
              handleDotCall(nextDotSourcePos, state, Vector(dotCall))
            }
            case _ => {
              Right((DotCall(terms.last, field, telescope, createMeta(Some(dotSourcePos), Some(state.sourcePos))), state))
            }
          }
        }

        parseNextTelescope(afterId)
      }
      case Right(Token.Operator(op, idSourcePos)) => {
        val afterOp = afterDot.advance()
        val field = ConcreteIdentifier(op, createMeta(Some(idSourcePos), Some(idSourcePos)))
        afterOp.current match {
          case Right(Token.LParen(_)) => {
            parseTuple(afterOp).map { case (args, afterArgs) =>
              (DotCall(terms.last, field, Vector(args), createMeta(Some(dotSourcePos), Some(afterArgs.sourcePos))), afterArgs)
            }
          }
          case _ => {
            Right((DotCall(terms.last, field, Vector.empty, createMeta(Some(dotSourcePos), Some(idSourcePos))), afterOp))
          }
        }
      }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def handleOperator(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    current = current.advance()
    parseAtom(current).flatMap { case (next, newState) =>
      current = newState
      val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ next
      current.current match {
        case Right(Token.Operator(nextOp, nextSourcePos)) => {
          handleOperator(nextOp, nextSourcePos, current, updatedTerms)
        }
        case _ => Right((next, current))
      }
    }
  }

  def parseAtom(current: LexerState): Either[ParseError, (Expr, LexerState)] = {
    current.current match {
      case Right(Token.LBrace(sourcePos)) => {
        // First check for empty object
        val afterBrace = current.advance()
        afterBrace.current match {
          case Right(Token.RBrace(_)) => {
            // Empty object
            parseObject(current)
          }
          case Right(Token.Identifier(_, _)) | Right(Token.SymbolLiteral(_, _)) => {
            // Look ahead one more token to see if it's followed by = or =>
            val afterId = afterBrace.advance()
            afterId.current match {
              case Right(Token.Operator(op, _)) if op == "=" || op == "=>" => {
                // Object field
                parseObject(current)
              }
              case _ => {
                // Not an object field, treat as block
                parseBlock(current)
              }
            }
          }
          case _ => {
            // Not an object field, treat as block
            parseBlock(current)
          }
        }
      }
      case Right(Token.LParen(sourcePos)) => {
        // Always parse parenthesized expressions as tuples
        parseTuple(current)
      }
      case Right(Token.Identifier(chars, sourcePos)) => {
        val afterId = current.advance()
        afterId.current match {
          case Right(Token.LBracket(_)) => {
            // Handle generic type parameters
            debug("parseAtom: Found generic type parameters after identifier")
            val identifier = ConcreteIdentifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos)))
            
            // Parse list of type parameters within square brackets
            parseList(afterId).flatMap { case (typeParams, afterTypeParams) =>
              // Now check if there are parentheses for function arguments
              afterTypeParams.current match {
                case Right(Token.LParen(_)) => {
                  // Function call with generic type parameters and arguments
                  parseTuple(afterTypeParams).map { case (tuple, afterArgs) =>
                    // Create nested function call: func[T](args) -> FunctionCall(FunctionCall(func, [T]), (args))
                    val funcWithGenericTypes = FunctionCall(
                      identifier,
                      typeParams,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )
                    (FunctionCall(
                      funcWithGenericTypes,
                      tuple,
                      createMeta(Some(sourcePos), Some(afterArgs.sourcePos))
                    ), afterArgs)
                  }
                }
                case _ => {
                  // Just the generic type parameters without function arguments
                  Right((FunctionCall(
                    identifier,
                    typeParams,
                    createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                  ), afterTypeParams))
                }
              }
            }
          }
          case Right(Token.LParen(_)) => {
            val identifier = ConcreteIdentifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos)))
            parseTuple(afterId).map { case (tuple, nextState) =>
              (FunctionCall(identifier, tuple, createMeta(Some(sourcePos), Some(sourcePos))), nextState)
            }
          }
          case _ => Right((ConcreteIdentifier(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), afterId))
        }
      }
      case Right(Token.IntegerLiteral(value, sourcePos)) => {
        val (numStr, base) = if (value.startsWith("0x")) {
          (value.drop(2), 16)
        } else if (value.startsWith("0b")) {
          (value.drop(2), 2)
        } else {
          (value, 10)
        }
        try {
          Right((ConcreteIntegerLiteral(BigInt(numStr, base), createMeta(Some(sourcePos), Some(sourcePos))), current.advance()))
        } catch {
          case e: NumberFormatException =>
            Left(ParseError(s"Invalid number format: $value", sourcePos.range.start))
        }
      }
      case Right(Token.RationalLiteral(value, sourcePos)) => {
        try {
          // Create a BigDecimal directly from the value string to match the V1 parser's behavior
          // This preserves exact representation of the number without extra manipulation
          val decimal = BigDecimal(value)
          val rational = spire.math.Rational(decimal)
          Right((ConcreteRationalLiteral(rational, createMeta(Some(sourcePos), Some(sourcePos))), current.advance()))
        } catch {
          case e: NumberFormatException =>
            Left(ParseError(s"Invalid floating-point number format: $value", sourcePos.range.start))
        }
      }
      case Right(Token.StringLiteral(chars, sourcePos)) =>
        Right((ConcreteStringLiteral(chars.map(_.text).mkString, createMeta(Some(sourcePos), Some(sourcePos))), current.advance()))
      case Right(Token.SymbolLiteral(value, sourcePos)) =>
        Right((chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(sourcePos), None)), current.advance()))
      case Right(Token.LBracket(sourcePos)) => {
        // Parse list
        parseList(current)
      }
      case Right(token) => Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))
      case Left(error) => Left(error)
    }
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    var current = skipComments(state)
    var exprs = Vector.empty[Expr]
    var maxExprs = 50

    while (exprs.length < maxExprs) {
      debug(s"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
      current.current match {
        case Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) => {
          debug("Found RParen after expression")
          return Right((exprs, current))
        }
        case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_)) => {
          debug("Found comment or whitespace, skipping")
          current = skipComments(current.advance())
        }
        case Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
          debug("Found comma or semicolon, skipping")
          current = skipComments(current.advance())
        }
        case _ => {
          debug("Parsing expression")
          parseExpr(current) match {
            case Right((expr, afterExpr)) => {
              current = skipComments(afterExpr)
              exprs = exprs :+ expr
              debug(s"After expression: maxExprs=$maxExprs, current token=${current.current}")
              current.current match {
                case Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) => {
                  debug("Found RParen after expression")
                  return Right((exprs, current))
                }
                case Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
                  debug("Found comma or semicolon after expression, advancing")
                  current = skipComments(current.advance())
                  maxExprs = maxExprs - 1
                }
                case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_)) => {
                  debug("Found comment or whitespace after expression, skipping")
                  current = skipComments(current.advance())
                }
                case Right(t)  => return Left(ParseError("Expected ',' or ')' after expression", t.sourcePos.range.start))
                case Left(err) => return Left(err)
              }
            }
            case Left(err) => return Left(err)
          }
        }
      }
    }
    Left(ParseError("Too many expressions in list", state.sourcePos.range.start))
  }

  def parseTuple(state: LexerState): Either[ParseError, (Tuple, LexerState)] = {
    state.current match {
      case Right(Token.LParen(sourcePos)) => {
        val afterLParen = skipComments(state.advance())
        parseExprList(afterLParen).flatMap { case (exprs, afterExprs) =>
          val afterList = skipComments(afterExprs)
          afterList.current match {
            case Right(Token.RParen(_)) => {
              // Always wrap expressions in a tuple when inside parentheses
              // This ensures type annotations are preserved
              val tupleExprs = exprs
              Right((Tuple(tupleExprs, createMeta(Some(sourcePos), Some(afterList.sourcePos))), afterList.advance()))
            }
            case Right(t) => Left(ParseError("Expected right parenthesis", t.sourcePos.range.start))
            case Left(err) => Left(err)
          }
        }
      }
      case Right(t) => Left(ParseError("Expected left parenthesis", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    var current = skipComments(state)
    var statements = Vector[Expr]()
    var result: Option[Expr] = None
    var maxExpressions = 100 // Prevent infinite loops

    // Skip the opening brace
    current.current match {
      case Right(Token.LBrace(sourcePos)) =>
        current = skipComments(current.advance())
      case Right(t) => return Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) => return Left(err)
    }

    // Regular block parsing - no special case for "case"
    while (maxExpressions > 0) {
      maxExpressions -= 1
      current.current match {
        case Right(Token.RBrace(_)) => {
          current = current.advance()
          return Right((Block(statements, result, None), current))
        }
        case Right(Token.Semicolon(_)) => {
          current = skipComments(current.advance())
        }
        case Right(Token.Whitespace(_)) => {
          current = skipComments(current.advance())
        }
        case _ => {
          parseExpr(current) match {
            case Left(err) => return Left(err)
            case Right((expr, next)) => {
              next.current match {
                case Right(Token.RBrace(_)) => {
                  result = Some(expr)
                  current = next.advance()
                  return Right((Block(statements, result, None), current))
                }
                case Right(Token.Semicolon(_)) | Right(Token.Whitespace(_)) => {
                  statements = statements :+ expr
                  current = skipComments(next)
                }
                case Right(t) => return Left(ParseError("Expected ';', whitespace, or '}' after expression in block", t.sourcePos.range.start))
                case Left(err) => return Left(err)
              }
            }
          }
        }
      }
    }
    Left(ParseError("Too many expressions in block", current.sourcePos.range.start))
  }

  def parseObject(current: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
    var state = current
    state.current match {
      case Right(Token.LBrace(sourcePos)) => {
        state = state.advance()
        var clauses = Vector.empty[ObjectClause]

        def parseField(): Either[ParseError, Unit] = {
          state.current match {
            case Right(Token.Identifier(chars, idSourcePos)) => {
              state = state.advance()
              val key = ConcreteIdentifier(chars.map(_.text).mkString, createMeta(Some(idSourcePos), Some(idSourcePos)))
              state.current match {
                case Right(Token.Operator(op, _)) => {
                  state = state.advance()
                  parseExpr(state).map { case (value, afterValue) =>
                    state = afterValue
                    clauses = clauses :+ ObjectExprClause(key, value)
                    state.current match {
                      case Right(Token.Comma(_))  => state = state.advance()
                      case Right(Token.RBrace(_)) => ()
                      case Right(t)               => return Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
                      case Left(err)              => return Left(err)
                    }
                  }
                }
                case Right(t)  => Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
                case Left(err) => Left(err)
              }
            }
            case Right(Token.SymbolLiteral(value, symSourcePos)) => {
              state = state.advance()
              val key = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
              state.current match {
                case Right(Token.Operator(op, _)) => {
                  state = state.advance()
                  parseExpr(state).map { case (value, afterValue) =>
                    state = afterValue
                    clauses = clauses :+ ObjectExprClauseOnValue(key, value)
                    state.current match {
                      case Right(Token.Comma(_))  => state = state.advance()
                      case Right(Token.RBrace(_)) => ()
                      case Right(t)               => return Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
                      case Left(err)              => return Left(err)
                    }
                  }
                }
                case Right(t)  => Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
                case Left(err) => Left(err)
              }
            }
            case Right(Token.RBrace(endPos)) => {
              state = state.advance()
              Right(())
            }
            case Right(t)  => Left(ParseError("Expected identifier, symbol literal or '}' in object", t.sourcePos.range.start))
            case Left(err) => Left(err)
          }
        }

        while (!state.current.exists(_.isInstanceOf[Token.RBrace])) {
          parseField() match {
            case Right(_)  => ()
            case Left(err) => return Left(err)
          }
        }

        state.current match {
          case Right(Token.RBrace(endPos)) => {
            state = state.advance()
            Right((ObjectExpr(clauses, createMeta(Some(sourcePos), Some(endPos))), state))
          }
          case Right(t)  => Left(ParseError("Expected '}' at end of object", t.sourcePos.range.start))
          case Left(err) => Left(err)
        }
      }
      case Right(t)  => Left(ParseError("Expected '{' at start of object", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def parseList(state: LexerState): Either[ParseError, (ListExpr, LexerState)] = {
    var current = skipComments(state)
    var exprs = Vector[Expr]()

    current.current match {
      case Right(Token.LBracket(sourcePos)) => {
        current = skipComments(current.advance())
        while (exprs.length < 50) {
          current.current match {
            case Right(Token.RBracket(endPos)) => {
              current = current.advance()
              return Right((ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))), current))
            }
            case Right(Token.Comma(_)) => {
              current = skipComments(current.advance())
            }
            case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_)) => {
              current = skipComments(current.advance())
            }
            case _ => {
              parseExpr(current) match {
                case Right((expr, afterExpr)) => {
                  current = skipComments(afterExpr)
                  exprs = exprs :+ expr
                  current.current match {
                    case Right(Token.RBracket(_)) => ()
                    case Right(Token.Comma(_)) => {
                      current = skipComments(current.advance())
                    }
                    case Right(t)  => return Left(ParseError("Expected ',' or ']' in list", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
                }
                case Left(err) => return Left(err)
              }
            }
          }
        }
        Left(ParseError("Too many elements in list", sourcePos.range.start))
      }
      case Right(t)  => Left(ParseError("Expected '[' at start of list", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def collectIdentifier(state: LexerState): (Vector[StringChar], LexerState) = {
    var chars = Vector.empty[StringChar]
    var currentState = state

    while (!currentState.isAtEnd && currentState.current.exists(token => token.isInstanceOf[Token.Identifier])) {
      currentState.current match {
        case Right(id: Token.Identifier) => {
          chars = chars ++ id.parts
          currentState = currentState.advance()
        }
        case _ => { throw new RuntimeException("Unreachable: exists check ensures this case never happens") }
      }
    }

    (chars, currentState)
  }

  def isIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(token) => { token.isInstanceOf[Token.Identifier] }
    case _            => { false }
  }

  def expectIdentifier(expected: String, state: LexerState): Either[ParseError, LexerState] = {
    state.current match {
      case Right(Token.Identifier(chars, _)) if chars.map(_.text).mkString == expected => {
        Right(advance())
      }
      case other => {
        Left(ParseError(s"Expected identifier '$expected' but got $other", state.sourcePos.range.start))
      }
    }
  }

  private def expectToken[T <: Token](state: LexerState)(using tag: ClassTag[T]): Either[ParseError, LexerState] = {
    state.current match {
      case Right(token) if token.isInstanceOf[T] => Right(state.advance())
      case Right(token) => Left(ParseError(s"Expected token of type ${tag.runtimeClass.getSimpleName}", token.sourcePos.range.start))
      case Left(err)    => Left(err)
    }
  }

  def isVarargContext(state: LexerState): Boolean = {
    // Check if we're in a function call argument list or type annotation
    var lookAhead = state.advance()
    lookAhead.current match {
      case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
      case _                                              => false
    }
  }

  def skipComments(state: LexerState): LexerState = {
    var current = state
    while (current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
      current = current.advance()
    }
    current
  }

  def parseFunctionCall(identifier: ConcreteIdentifier, sourcePos: SourcePos, current: LexerState): Either[ParseError, (Expr, LexerState)] = {
    val afterLParen = current.advance()
    parseExprList(afterLParen) match {
      case Left(error) => Left(error)
      case Right((listExpr, afterList)) => {
        afterList.current match {
          case Right(Token.RParen(endPos)) => {
            Right((FunctionCall(identifier, Tuple(listExpr.toVector, createMeta(Some(sourcePos), Some(endPos))), createMeta(Some(sourcePos), Some(endPos))), afterList.advance()))
          }
          case Right(t) => Left(ParseError("Expected right parenthesis", t.sourcePos.range.start))
          case Left(err) => Left(err)
        }
      }
    }
  }
}
