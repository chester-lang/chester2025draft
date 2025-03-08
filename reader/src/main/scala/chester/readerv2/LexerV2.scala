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

  // Keep DEBUG flag for tests that use it
  var DEBUG = false 
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.DEBUG

  private def debug(msg: => String): Unit = if (DEBUG) {
    println(s"[DEBUG] $msg")
  }

  private var state: LexerState = LexerState(tokens.toVector, 0)

  // Helper method to convert a sequence of StringChar to a string
  private def charsToString(chars: Seq[StringChar]): String = chars.map(_.text).mkString

  // Helper method to create a common ParseError from current token
  private def createParseError(msg: String, token: Either[ParseError, Token]): ParseError = token match {
    case Right(t) => ParseError(msg, t.sourcePos.range.start)
    case Left(err) => err
  }

  // Helper method for common error cases
  private def expectedError(expected: String, token: Either[ParseError, Token]): ParseError = 
    createParseError(s"Expected $expected", token)

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

  private def advance(): LexerState = {
    // Simply increment the index
    LexerState(state.tokens, state.index + 1)
  }

  // See docs/src/dev/parser-migration.md for parser design documentation
  type LexerError = ParseError

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Replace skipComments with collectComments to preserve comments
    val (leadingComments, current) = collectComments(state)
    var terms = Vector.empty[Expr]
    debug(s"Starting parseExpr with state: $current")

    def buildOpSeq(terms: Vector[Expr]): Either[ParseError, Expr] = {
      debug(s"Building OpSeq with terms: $terms")
      if (terms.isEmpty) {
        Left(ParseError("Empty operator sequence", getStartPos(state.current)))
      } else if (terms.length == 1) {
        val expr = terms.head
        // Attach any leading comments to the single expression
        if (leadingComments.nonEmpty) {
          Right(expr.updateMeta { meta =>
            val newMeta = createMetaWithComments(
              meta.flatMap(_.sourcePos),
              leadingComments
            )
            
            // Merge existing meta with new comment info
            (meta, newMeta) match {
              case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
                val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
                val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
                  case (Some(existingInfo), Some(newInfo)) => 
                    Some(chester.syntax.concrete.CommentInfo(
                      commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                      commentInBegin = existingInfo.commentInBegin,
                      commentInEnd = existingInfo.commentInEnd,
                      commentEndInThisLine = existingInfo.commentEndInThisLine
                    ))
                  case (None, commentInfo) => commentInfo
                  case (commentInfo, None) => commentInfo
                }
                Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
              case (None, meta) => meta
              case (meta, None) => meta
            }
          })
        } else {
          Right(expr)
        }
      } else {
        // Create OpSeq with the leading comments
        val opSeqMeta = if (leadingComments.nonEmpty) {
          Some(createMetaWithComments(None, leadingComments).get)
        } else {
          None
        }
        Right(OpSeq(terms, opSeqMeta))
      }
    }

    def parseRest(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
      // Create local terms vector starting with the input expression
      var localTerms = Vector(expr)
      
      debug(s"parseRest called with expr: $expr, state: $state, current terms: $localTerms")
      // Collect comments but don't attach them yet
      val (restComments, current) = collectComments(state)
      current.current match {
        case Right(Token.EOF(_)) | Right(Token.RParen(_)) | Right(Token.RBrace(_)) | Right(Token.RBracket(_)) | Right(Token.Comma(_)) | Right(Token.Semicolon(_)) => {
          debug("parseRest: Hit terminator token")
          buildOpSeq(localTerms).map(result => (result, current))
        }
        case Right(Token.LBrace(braceSourcePos)) => {
          debug("parseRest: Found LBrace after expression, treating as block argument")
          // The LBrace is a block argument to the previous expression
          parseBlockWithComments(current).flatMap { case (block, afterBlock) =>
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
              // If previous expression is an identifier, handle based on identifier value
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
            if ((newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[chester.syntax.concrete.Identifier]) || 
                (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[FunctionCall])) {
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
          parseAtomWithComments(afterColon).flatMap { case (next, afterNext) =>
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
            parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
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
          val text = charsToString(chars)
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
              parseBlockWithComments(afterId).flatMap { case (block, afterBlock) =>
                // In V1 parser, a block after an identifier in an infix expression is treated as part of the OpSeq
                // not as a function call argument
                val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
                localTerms = localTerms :+ id
                localTerms = localTerms :+ block
                debug(s"parseRest: After block in infix, terms: $localTerms")
                parseRest(block, afterBlock).map { case (result, finalState) =>
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
              parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
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
          parseAtomWithComments(current).flatMap { case (next, afterNext) =>
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
            parseAtomWithComments(afterOp).flatMap { case (expr, afterExpr) =>
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
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) => {
        // Handle keyword operators like "not", "if", etc. as prefix operators
        debug(s"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        val afterOp = current.advance()
        // Create an operator term
        terms = Vector(ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))))
        parseAtomWithComments(afterOp).flatMap { case (expr, afterExpr) =>
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
        parseAtomWithComments(current).flatMap { case (first, afterFirst) =>
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
        val field = ConcreteIdentifier(charsToString(chars1), createMeta(Some(idSourcePos1), Some(idSourcePos1)))
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
    // Advance once and parse the next atom
    parseAtomWithComments(state.advance()).flatMap { case (next, newState) =>
      val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ next
      newState.current match {
        case Right(Token.Operator(nextOp, nextSourcePos)) => {
          handleOperator(nextOp, nextSourcePos, newState, updatedTerms)
        }
        case _ => Right((next, newState))
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
          case Right(Token.Identifier(_, _)) | Right(Token.SymbolLiteral(_, _)) | Right(Token.StringLiteral(_, _)) => {
            // Look ahead one more token to see if it's followed by = or =>
            val afterId = afterBrace.advance()
            afterId.current match {
              case Right(Token.Operator(op, _)) if op == "=" || op == "=>" => {
                // Object field
                parseObject(current)
              }
              case _ => {
                // Not an object field, treat as block
                parseBlockWithComments(current)
              }
            }
          }
          case _ => {
            // Not an object field, treat as block
            parseBlockWithComments(current)
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
            val identifier = ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos)))
            
            // Parse list of type parameters within square brackets
            parseListWithComments(afterId).flatMap { case (typeParams: ListExpr, afterTypeParams) =>
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
            val identifier = ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos)))
            parseTuple(afterId).map { case (tuple, nextState) =>
              (FunctionCall(identifier, tuple, createMeta(Some(sourcePos), Some(sourcePos))), nextState)
            }
          }
          case _ => Right((ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))), afterId))
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
        Right((ConcreteStringLiteral(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))), current.advance()))
      case Right(Token.SymbolLiteral(value, sourcePos)) =>
        Right((chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(sourcePos), None)), current.advance()))
      case Right(Token.LBracket(sourcePos)) => {
        // Parse list
        parseListWithComments(current)
      }
      case Right(token) => Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))
      case Left(error) => Left(error)
    }
  }

  // Helper method to check if a token is a terminator (right delimiter or comma/semicolon)
  private def isTerminator(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket | 
         _: Token.Comma | _: Token.Semicolon => true
    case _ => false
  }

  // Helper method to check if a token is specifically a right delimiter
  private def isRightDelimiter(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket => true
    case _ => false
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    // Replace skipComments with collectComments to preserve comments
    val (leadingListComments, initialState) = collectComments(state)
    var current = initialState
    var exprs = Vector.empty[Expr]
    var maxExprs = 50

    while (exprs.length < maxExprs) {
      debug(s"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
      current.current match {
        case Right(token) if isRightDelimiter(token) => {
          debug("Found right delimiter after expression")
          return Right((exprs, current))
        }
        case Right(_: Token.Comment | _: Token.Whitespace) => {
          // Collect comments instead of skipping them
          val (comments, afterComments) = collectComments(current)
          current = afterComments
        }
        case Right(_: Token.Comma | _: Token.Semicolon) => {
          debug("Found comma or semicolon, skipping")
          // Collect any comments after comma/semicolon
          val (_, afterDelimiter) = collectComments(current.advance())
          current = afterDelimiter
        }
        case _ => {
          debug("Parsing expression")
          parseExpr(current) match {
            case Right((expr, afterExpr)) => {
              // Collect comments after the expression
              val (trailingComments, afterComments) = collectComments(afterExpr)
              
              // Attach trailing comments to the expression if any
              val updatedExpr = if (trailingComments.nonEmpty) {
                expr.updateMeta { meta =>
                  val newMeta = createMetaWithComments(
                    meta.flatMap(_.sourcePos),
                    Vector.empty,
                    trailingComments
                  )
                  
                  // Merge with existing meta
                  (meta, newMeta) match {
                    case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
                      val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
                      val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
                        case (Some(existingInfo), Some(newInfo)) => 
                          Some(chester.syntax.concrete.CommentInfo(
                            commentBefore = existingInfo.commentBefore,
                            commentInBegin = existingInfo.commentInBegin,
                            commentInEnd = existingInfo.commentInEnd,
                            commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
                          ))
                        case (None, commentInfo) => commentInfo
                        case (commentInfo, None) => commentInfo
                      }
                      Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
                    case (None, meta) => meta
                    case (meta, None) => meta
                  }
                }
              } else {
                expr
              }
              
              current = afterComments
              exprs = exprs :+ updatedExpr
              
              debug(s"After expression: maxExprs=$maxExprs, current token=${current.current}")
              current.current match {
                case Right(token) if isRightDelimiter(token) => {
                  debug("Found right delimiter after expression")
                  return Right((exprs, current))
                }
                case Right(_: Token.Comma | _: Token.Semicolon) => {
                  debug("Found comma or semicolon after expression, advancing")
                  // Collect any comments after comma/semicolon
                  val (_, afterDelimiter) = collectComments(current.advance())
                  current = afterDelimiter
                  maxExprs = maxExprs - 1
                }
                case Right(_: Token.Comment | _: Token.Whitespace) => {
                  // Collect comments instead of skipping them
                  val (comments, afterComments) = collectComments(current)
                  current = afterComments
                }
                case Right(t) => return Left(expectedError("',' or ')' after expression", Right(t)))
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
        // Use collectComments instead of skipComments
        val (leadingComments, afterLParen) = collectComments(state.advance())
        parseExprList(afterLParen).flatMap { case (exprs, afterExprs) =>
          // Use collectComments instead of skipComments
          val (trailingComments, afterList) = collectComments(afterExprs)
          afterList.current match {
            case Right(Token.RParen(_)) => {
              // Always wrap expressions in a tuple when inside parentheses
              // This ensures type annotations are preserved
              val tupleExprs = exprs
              
              // Create meta with comments
              val tupleMeta = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(afterList.sourcePos))
                meta.map { m =>
                  val commentInfo = (leadingComments.nonEmpty, trailingComments.nonEmpty) match {
                    case (true, true) => 
                      Some(chester.syntax.concrete.CommentInfo(
                        commentBefore = leadingComments,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = trailingComments
                      ))
                    case (true, false) => 
                      Some(chester.syntax.concrete.CommentInfo(
                        commentBefore = leadingComments,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = Vector.empty
                      ))
                    case (false, true) => 
                      Some(chester.syntax.concrete.CommentInfo(
                        commentBefore = Vector.empty,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = trailingComments
                      ))
                    case _ => None
                  }
                  
                  ExprMeta(m.sourcePos, commentInfo)
                }
              } else {
                createMeta(Some(sourcePos), Some(afterList.sourcePos))
              }
              
              Right((Tuple(tupleExprs, tupleMeta), afterList.advance()))
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
    // Replace skipComments with collectComments
    val (_, current) = collectComments(state)
    var statements = Vector[Expr]()
    var result: Option[Expr] = None
    var maxExpressions = 100 // Prevent infinite loops

    // Skip the opening brace
    current.current match {
      case Right(Token.LBrace(sourcePos)) => {
        // Use collectComments instead of skipComments
        val (blockStartComments, afterBrace) = collectComments(current.advance())
        
        // Create a local state to track comments and expressions
        var blockCurrent = afterBrace
        
        // Regular block parsing - no special case for "case"
        while (maxExpressions > 0) {
          maxExpressions -= 1
          blockCurrent.current match {
            case Right(Token.RBrace(_)) => {
              val finalBlock = Block(statements, result, None)
              blockCurrent = blockCurrent.advance()
              return Right((finalBlock, blockCurrent))
            }
            case Right(Token.Semicolon(_)) => {
              // Collect any comments after semicolon
              val (_, afterSemi) = collectComments(blockCurrent.advance())
              blockCurrent = afterSemi
            }
            case Right(Token.Whitespace(_)) => {
              // Collect comments instead of skipping
              val (_, afterWs) = collectComments(blockCurrent.advance())
              blockCurrent = afterWs
            }
            case _ => {
              parseExpr(blockCurrent) match {
                case Left(err) => return Left(err)
                case Right((expr, next)) => {
                  next.current match {
                    case Right(Token.RBrace(_)) => {
                      result = Some(expr)
                      blockCurrent = next.advance()
                      return Right((Block(statements, result, None), blockCurrent))
                    }
                    case Right(Token.Semicolon(_)) | Right(Token.Whitespace(_)) => {
                      statements = statements :+ expr
                      // Collect comments after statement
                      val (_, afterStmt) = collectComments(next)
                      blockCurrent = afterStmt
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
      case Right(t) => return Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) => return Left(err)
    }
  }

  def parseObject(current: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
    // Collect comments before the object
    val (leadingComments, initialState) = collectComments(current)
    var state = initialState
    
    state.current match {
      case Right(Token.LBrace(sourcePos)) => {
        // Collect comments after the opening brace
        val (afterBraceComments, afterBrace) = collectComments(state.advance())
        state = afterBrace
        var clauses = Vector.empty[ObjectClause]

        // Helper function to check for comma or right brace after object field
        def checkAfterField(): Either[ParseError, Unit] = {
          state.current match {
            case Right(Token.Comma(_))  => { 
              // Collect comments after comma
              val (_, afterComma) = collectComments(state.advance()) 
              state = afterComma
              Right(()) 
            }
            case Right(Token.RBrace(_)) => Right(())
            case Right(t)               => Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
            case Left(err)              => Left(err)
          }
        }

        // Helper function to handle field value after operator
        def handleFieldValue(key: Expr, op: String, keySourcePos: SourcePos): Either[ParseError, Unit] = {
          parseExpr(state).map { case (value, afterValue) =>
            state = afterValue
            if (op == "=>") {
              clauses = clauses :+ ObjectExprClauseOnValue(key, value)
            } else { // op == "="
              // For string literals with "=", convert to identifier
              key match {
                case stringLit: ConcreteStringLiteral =>
                  val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                  clauses = clauses :+ ObjectExprClause(idKey, value)
                case qualifiedName: QualifiedName =>
                  clauses = clauses :+ ObjectExprClause(qualifiedName, value)
                case other => 
                  // This shouldn't happen with proper validation upstream
                  return Left(ParseError(s"Expected identifier for object field key with = operator but got: $other", 
                                        keySourcePos.range.start))
              }
            }
            checkAfterField()
          }
        }

        def parseField(): Either[ParseError, Unit] = {
          state.current match {
            case Right(Token.Identifier(chars, idSourcePos)) => {
              state = state.advance()
              val key = ConcreteIdentifier(charsToString(chars), createMeta(Some(idSourcePos), Some(idSourcePos)))
              state.current match {
                case Right(Token.Operator(op, _)) => {
                  state = state.advance()
                  handleFieldValue(key, op, idSourcePos)
                }
                case Right(t)  => Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
                case Left(err) => Left(err)
              }
            }
            case Right(Token.StringLiteral(chars, strSourcePos)) => {
              state = state.advance()
              val keyLiteral = ConcreteStringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
              state.current match {
                case Right(Token.Operator(op, _)) => {
                  state = state.advance()
                  handleFieldValue(keyLiteral, op, strSourcePos)
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
                  handleFieldValue(key, op, symSourcePos)
                }
                case Right(t)  => Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
                case Left(err) => Left(err)
              }
            }
            case Right(Token.RBrace(endPos)) => {
              state = state.advance()
              Right(())
            }
            case Right(t)  => Left(ParseError("Expected identifier, string literal, symbol literal or '}' in object", t.sourcePos.range.start))
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
            // Create meta with comments
            val objectMeta = if (leadingComments.nonEmpty || afterBraceComments.nonEmpty) {
              val meta = createMeta(Some(sourcePos), Some(endPos))
              meta.map { m =>
                val allLeadingComments = leadingComments ++ afterBraceComments
                val commentInfo = if (allLeadingComments.nonEmpty) {
                  Some(chester.syntax.concrete.CommentInfo(
                    commentBefore = allLeadingComments,
                    commentInBegin = Vector.empty,
                    commentInEnd = Vector.empty,
                    commentEndInThisLine = Vector.empty
                  ))
                } else {
                  None
                }
                
                ExprMeta(m.sourcePos, commentInfo)
              }
            } else {
              createMeta(Some(sourcePos), Some(endPos))
            }
            
            state = state.advance()
            Right((ObjectExpr(clauses, objectMeta), state))
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
    // Replace skipComments with collectComments
    val (leadingComments, initialState) = collectComments(state)
    var current = initialState
    var exprs = Vector[Expr]()

    current.current match {
      case Right(Token.LBracket(sourcePos)) => {
        val (afterBracketComments, afterBracket) = collectComments(current.advance())
        current = afterBracket
        
        while (exprs.length < 50) {
          current.current match {
            case Right(Token.RBracket(endPos)) => {
              // Add comments to list meta
              val listMeta = if (leadingComments.nonEmpty || afterBracketComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map { m =>
                  val allLeadingComments = leadingComments ++ afterBracketComments
                  val commentInfo = if (allLeadingComments.nonEmpty) {
                    Some(chester.syntax.concrete.CommentInfo(
                      commentBefore = allLeadingComments
                    ))
                  } else {
                    None
                  }
                  
                  ExprMeta(m.sourcePos, commentInfo)
                }
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }
              
              current = current.advance()
              return Right((ListExpr(exprs, listMeta), current))
            }
            case Right(Token.Comma(_)) => {
              // Collect comments after comma
              val (_, afterComma) = collectComments(current.advance())
              current = afterComma
            }
            case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_)) => {
              // Collect comments
              val (_, afterComments) = collectComments(current)
              current = afterComments
            }
            case _ => {
              parseExpr(current) match {
                case Right((expr, afterExpr)) => {
                  // Collect any comments after the expression
                  val (_, afterComments) = collectComments(afterExpr)
                  current = afterComments
                  exprs = exprs :+ expr
                  
                  current.current match {
                    case Right(Token.RBracket(_)) => ()
                    case Right(Token.Comma(_)) => {
                      val (_, afterComma) = collectComments(current.advance())
                      current = afterComma
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
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == expected => {
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

  /**
   * Collects comments from the current state.
   * Returns a tuple of (collected comments, updated state).
   */
  def collectComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    var comments = Vector.empty[chester.syntax.concrete.Comment]
    var current = state
    
    while (!current.isAtEnd && 
           current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
      current.current match {
        case Right(Token.Comment(text, sourcePos)) =>
          val commentType = if (text.trim.startsWith("//")) {
            chester.syntax.concrete.CommentType.OneLine
          } else {
            chester.syntax.concrete.CommentType.MultiLine
          }
          
          val comment = chester.syntax.concrete.Comment(
            content = text.trim,
            typ = commentType,
            sourcePos = Some(sourcePos)
          )
          comments = comments :+ comment
          current = current.advance()
        case Right(Token.Whitespace(_)) =>
          // In Whitespace tokens, we don't have the actual text content
          // Just advance the token - we'll hit another token eventually
          current = current.advance()
        case _ => 
          // Should never happen due to the while condition
          current = current.advance()
      }
    }
    
    (comments, current)
  }

  /**
   * Collects trailing comments after an expression until a newline or non-comment token.
   */
  def collectTrailingComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    // For trailing comments, we only collect comments that appear on the same line
    // (until we hit a newline in whitespace)
    var comments = Vector.empty[chester.syntax.concrete.Comment]
    var current = state
    var hitNewline = false
    
    while (!current.isAtEnd && !hitNewline &&
           current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
      current.current match {
        case Right(Token.Comment(text, sourcePos)) =>
          val commentType = if (text.trim.startsWith("//")) {
            chester.syntax.concrete.CommentType.OneLine
          } else {
            chester.syntax.concrete.CommentType.MultiLine
          }
          
          val comment = chester.syntax.concrete.Comment(
            content = text.trim,
            typ = commentType,
            sourcePos = Some(sourcePos)
          )
          comments = comments :+ comment
          current = current.advance()
        case Right(Token.Whitespace(_)) =>
          // In Whitespace tokens, we don't have the actual text content
          // Just assume any whitespace might contain a newline and stop collecting
          hitNewline = true
          current = current.advance()
        case _ => 
          // Should never happen due to the while condition
          current = current.advance()
      }
    }
    
    (comments, current)
  }

  /**
   * Creates ExprMeta with comments.
   */
  def createMetaWithComments(
    sourcePos: Option[SourcePos],
    leadingComments: Vector[chester.syntax.concrete.Comment],
    trailingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty
  ): Option[ExprMeta] = {
    if (sourcePos.isEmpty && leadingComments.isEmpty && trailingComments.isEmpty) {
      None
    } else {
      val commentInfo = if (leadingComments.isEmpty && trailingComments.isEmpty) {
        None
      } else {
        Some(chester.syntax.concrete.CommentInfo(
          commentBefore = leadingComments,
          commentEndInThisLine = trailingComments
        ))
      }
      Some(ExprMeta(sourcePos, commentInfo))
    }
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

  /**
   * Parse an atom expression with comments.
   * This collects leading and trailing comments and attaches them to the expression.
   */
  def parseAtomWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments(state)
    
    // Parse the actual expression
    parseAtom(afterLeadingComments).flatMap { case (expr, afterExpr) =>
      // Collect trailing comments
      val (trailingComments, finalState) = collectTrailingComments(afterExpr)
      
      // Update expression with comments
      val updatedExpr = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )
          
          // Merge the existing meta with new comment information
          (existingMeta, newMeta) match {
            case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
              val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
              val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
                case (Some(existingInfo), Some(newInfo)) => 
                  Some(chester.syntax.concrete.CommentInfo(
                    commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                    commentInBegin = existingInfo.commentInBegin,
                    commentInEnd = existingInfo.commentInEnd,
                    commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
                  ))
                case (None, commentInfo) => commentInfo
                case (commentInfo, None) => commentInfo
              }
              Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
            case (None, meta) => meta
            case (meta, None) => meta
          }
        }
      } else {
        expr
      }
      
      Right((updatedExpr, finalState))
    }
  }

  /**
   * Parse a block with comments.
   */
  def parseBlockWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments(state)
    
    // Parse the block
    parseBlock(afterLeadingComments).flatMap { case (block, afterBlock) =>
      // Collect trailing comments after the block
      val (trailingComments, finalState) = collectTrailingComments(afterBlock)
      
      // Update block with comments
      val updatedBlock = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        block.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )
          
          // Merge the existing meta with new comment information
          (existingMeta, newMeta) match {
            case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
              val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
              val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
                case (Some(existingInfo), Some(newInfo)) => 
                  Some(chester.syntax.concrete.CommentInfo(
                    commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                    commentInBegin = existingInfo.commentInBegin,
                    commentInEnd = existingInfo.commentInEnd,
                    commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
                  ))
                case (None, commentInfo) => commentInfo
                case (commentInfo, None) => commentInfo
              }
              Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
            case (None, meta) => meta
            case (meta, None) => meta
          }
        }
      } else {
        block
      }
      
      Right((updatedBlock, finalState))
    }
  }

  /**
   * Parse a list with comments.
   */
  def parseListWithComments(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments(state)
    
    // Parse the list using the original parseList method
    parseList(afterLeadingComments).flatMap { case (list, afterList) =>
      // Collect trailing comments after the list
      val (trailingComments, finalState) = collectTrailingComments(afterList)
      
      // Update list with comments
      val updatedList = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        list.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )
          
          // Merge the existing meta with new comment information
          (existingMeta, newMeta) match {
            case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
              val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
              val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
                case (Some(existingInfo), Some(newInfo)) => 
                  Some(chester.syntax.concrete.CommentInfo(
                    commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                    commentInBegin = existingInfo.commentInBegin,
                    commentInEnd = existingInfo.commentInEnd,
                    commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
                  ))
                case (None, commentInfo) => commentInfo
                case (commentInfo, None) => commentInfo
              }
              Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
            case (None, meta) => meta
            case (meta, None) => meta
          }
        }
      } else {
        list
      }
      
      Right((updatedList, finalState))
    }
  }
}
