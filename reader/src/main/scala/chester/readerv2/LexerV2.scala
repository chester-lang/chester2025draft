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
import chester.error.{Pos, RangeInFile, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.{
  Block,
  DotCall,
  Expr,
  ExprMeta,
  FunctionCall,
  ListExpr,
  ObjectClause,
  ObjectExpr,
  ObjectExprClause,
  ObjectExprClauseOnValue,
  OpSeq,
  QualifiedName,
  Tuple
}
import chester.syntax.concrete.{
  Identifier as ConcreteIdentifier,
  IntegerLiteral as ConcreteIntegerLiteral,
  RationalLiteral as ConcreteRationalLiteral,
  StringLiteral as ConcreteStringLiteral
}
import chester.syntax.concrete.Literal.*
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import chester.error.*
import chester.reader.*
import chester.syntax.*
import chester.syntax.concrete.*

import scala.annotation.tailrec

// Token extractors for cleaner pattern matching
private object TokenExtractors {
  import chester.readerv2.Token.*
  // Define extractors using pattern matching
  object Id {
    def unapply(token: Either[ParseError, Token]): Option[(Vector[StringChar], SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.Identifier(chars, pos)) => (chars, pos)
    }
  }

  object Op {
    def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.Operator(op, pos)) => (op, pos)
    }
  }

  object Str {
    def unapply(token: Either[ParseError, Token]): Option[(Vector[StringChar], SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.StringLiteral(value, pos)) => (value, pos)
    }
  }

  object Sym {
    def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.SymbolLiteral(value, pos)) => (value, pos)
    }
  }

  object Int {
    def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.IntegerLiteral(value, pos)) => (value, pos)
    }
  }

  object Rat {
    def unapply(token: Either[ParseError, Token]): Option[(String, SourcePos)] = PartialFunction.condOpt(token) {
      case Right(Token.RationalLiteral(value, pos)) => (value, pos)
    }
  }

  // Helper for source position extractors - consolidated into a single implementation
  private def posExtract(tokenPredicate: Token => Boolean): Either[ParseError, Token] => Option[SourcePos] = {
    case Right(token) if tokenPredicate(token) => Some(token.sourcePos)
    case _                                     => None
  }

  // Define all delimiter token extractors using the posExtract helper
  object LParen {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LParen])(t)
  }

  object RParen {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RParen])(t)
  }

  object LBrace {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LBrace])(t)
  }

  object RBrace {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RBrace])(t)
  }

  object LBracket {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.LBracket])(t)
  }

  object RBracket {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.RBracket])(t)
  }

  object Comma {
    def unapply(t: Either[ParseError, Token]): Option[SourcePos] = posExtract(_.isInstanceOf[Token.Comma])(t)
  }

  object Err {
    def unapply(token: Either[ParseError, Token]): Option[ParseError] = PartialFunction.condOpt(token) { case Left(err) =>
      err
    }
  }
}

import TokenExtractors._

case class LexerState(
    tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    newLineAfterBlockMeansEnds: Boolean = false
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = current match {
    case Right(token) => LexerState(tokens, index + 1, Some(token), newLineAfterBlockMeansEnds)
    case Left(_)      => LexerState(tokens, index + 1, previousToken, newLineAfterBlockMeansEnds)
  }
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t)  => t.sourcePos
  }

  // Helper methods for common state checks
  def isAtTerminator: Boolean = current.exists(t =>
    t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] ||
      t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] ||
      t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon]
  )

  // Helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState =
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)

  override def toString: String =
    s"LexerState(index=$index, current=$current, previousToken=$previousToken, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  def apply(sourceOffset: SourceOffset, ignoreLocation: Boolean = false) =
    new LexerV2(sourceOffset, ignoreLocation)

  var DEBUG = false // Keep DEBUG flag for tests that use it
  private val MAX_LIST_ELEMENTS = 50 // Constants for parser configuration
}

import LexerV2.DEBUG

class LexerV2(sourceOffset: SourceOffset, ignoreLocation: Boolean) {

  private def debug(msg: => String): Unit = if (DEBUG) println(s"[DEBUG] $msg")

  // Helper methods
  private def charsToString(chars: Seq[StringChar]): String = chars.map(_.text).mkString

  private def expectedError(expected: String, token: Either[ParseError, Token]): ParseError = {
    def getTokenType(t: Token): String = t match {
      case _: Token.Identifier      => "identifier"
      case _: Token.IntegerLiteral  => "integer literal"
      case _: Token.RationalLiteral => "rational literal"
      case _: Token.StringLiteral   => "string literal"
      case _: Token.Operator        => "operator"
      case _: Token.LParen          => "left parenthesis '('"
      case _: Token.RParen          => "right parenthesis ')'"
      case _: Token.LBrace          => "left brace '{'"
      case _: Token.RBrace          => "right brace '}'"
      case _: Token.LBracket        => "left bracket '['"
      case _: Token.RBracket        => "right bracket ']'"
      case _: Token.Colon           => "colon ':'"
      case _: Token.Comma           => "comma ','"
      case _: Token.Dot             => "dot '.'"
      case _: Token.Semicolon       => "semicolon ';'"
      case _: Token.EOF             => "end of file"
      case _: Token.Whitespace      => "whitespace"
      case _                        => "unknown token"
    }

    token.fold(
      identity,
      t =>
        ParseError(
          s"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
          t.sourcePos.range.start
        )
    )
  }

  /** Creates expression metadata from source positions and comments. */
  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] =
    if (ignoreLocation) None
    else
      PartialFunction.condOpt((startPos, endPos)) {
        case (Some(start), Some(end)) =>
          ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None)
        case (Some(pos), None) =>
          ExprMeta(Some(pos), None)
        case (None, Some(pos)) =>
          ExprMeta(Some(pos), None)
      }

  private def getStartPos(token: Either[ParseError, Token]): Pos =
    token.fold(_.pos, _.sourcePos.range.start)

  private def mergeMeta(existing: Option[ExprMeta], newMeta: Option[ExprMeta]): Option[ExprMeta] =
    (existing, newMeta) match {
      case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
        val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
        val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
          case (Some(existingInfo), Some(newInfo)) =>
            Some(
              CommentInfo(
                commentBefore = existingInfo.commentBefore ++ newInfo.commentBefore,
                commentInBegin = existingInfo.commentInBegin,
                commentInEnd = existingInfo.commentInEnd,
                commentEndInThisLine = existingInfo.commentEndInThisLine ++ newInfo.commentEndInThisLine
              )
            )
          case (None, commentInfo) => commentInfo
          case (commentInfo, None) => commentInfo
        }
        Some(ExprMeta(mergedSourcePos, mergedCommentInfo))
      case (None, meta) => meta
      case (meta, None) => meta
    }

  // Main expression continuation parser
  def parseRest(
      expr: Expr,
      state: LexerState
  )(leadingComments: Vector[Comment] = Vector.empty): Either[ParseError, (Expr, LexerState)] = {
    debug(s"REST: Called with expr: $expr, state: $state, token: ${state.current}")
    
    // If we're at a terminator, return the expression directly
    if (state.isAtTerminator) {
      debug("REST: Hit terminator token, returning expression directly")
      Right((expr, state))
    } else {
      state.current match {
        // Block argument handling
        case Right(Token.LBrace(braceSourcePos)) =>
          debug("REST: Found LBrace after expression, treating as block argument")
          handleBlockArgument(expr, state)(leadingComments)

        // Colon handling (type annotations, etc)
        case Right(Token.Colon(sourcePos)) =>
          debug("REST: Found colon, handling type annotation")
          handleColon(sourcePos, state, Vector(expr))(leadingComments)

        // Dot call handling
        case Right(Token.Dot(dotSourcePos)) =>
          debug("REST: Found dot, handling method call")
          handleDotCall(dotSourcePos, state, Vector(expr)).flatMap { case (dotCall, newState) =>
            parseRest(dotCall, newState)(leadingComments)
          }

        // Operator handling - treat all operators uniformly
        case Right(Token.Operator(op, sourcePos)) =>
          debug(s"REST: Found operator $op, handling uniformly")
          val afterOp = state.advance()
          val opId = ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))
          
          // Add operator to terms
          val terms = Vector(expr, opId)
          
          // For operator followed by blocks, ensure blocks are handled directly
          afterOp.current match {
            case Right(Token.LBrace(_)) =>
              debug(s"REST: Found block directly after operator $op, parsing block")
              withComments(parseBlock)(afterOp).map { case (block, afterBlock) =>
                debug(s"REST: After parsing block, block=$block")
                val blockTerms = terms :+ block
                debug(s"REST: Created block terms: $blockTerms")
                (OpSeq(blockTerms, None), afterBlock)
              }
              
            case _ =>
              debug(s"REST: No block after operator $op, parsing next atom")
              // Continue parsing the rest of the expression for non-block atoms
              withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
                debug(s"REST: After parsing atom after operator, got: $next")
                val newTerms = terms :+ next
                debug(s"REST: New terms after operator: $newTerms")
                
                if (afterNext.isAtTerminator) {
                  debug(s"REST: At terminator after operator sequence: $newTerms")
                  Right((OpSeq(newTerms, None), afterNext))
                } else {
                  debug(s"REST: Not at terminator, continuing parsing")
                  // Continue parsing the rest
                  parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
                    debug(s"REST: After recursive parseRest, result=$result")
                    result match {
                      case opSeq: OpSeq =>
                        debug(s"REST: Result is OpSeq, combining terms")
                        (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                      case _ =>
                        debug(s"REST: Result is not OpSeq, using newTerms")
                        (OpSeq(newTerms, None), finalState)
                    }
                  }
                }
              }
          }

        // Identifier handling
        case Right(Token.Identifier(parts, sourcePos)) =>
          val text = charsToString(parts)
          debug(s"REST: Found identifier $text")
          val idExpr = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val afterId = state.advance()
          
          // Check if there's a brace right after this identifier
          if (afterId.current.exists(_.isInstanceOf[Token.LBrace])) {
            debug(s"REST: Found brace after identifier in infix")
            withComments(parseBlock)(afterId).flatMap { case (block, afterBlock) =>
              val updatedTerms = Vector(expr, idExpr, block)
              debug(s"REST: After parsing block after identifier, terms: $updatedTerms")
              Right((OpSeq(updatedTerms, None), afterBlock))
            }
          } else {
            // Build the OpSeq with the new identifier and continue parsing
            val updatedTerms = Vector(expr, idExpr)
            debug(s"REST: After adding identifier, terms: $updatedTerms")
            parseRest(idExpr, afterId)(leadingComments).map { case (result, finalState) =>
              debug(s"REST: After recursive parseRest with identifier, result=$result")
              result match {
                case opSeq: OpSeq =>
                  debug(s"REST: Result is OpSeq, combining with expr")
                  (OpSeq(Vector(expr) ++ opSeq.seq, None), finalState)
                case _ =>
                  debug(s"REST: Result is not OpSeq, using updatedTerms")
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }

        // Generic token handling
        case Right(_) =>
          debug("REST: Found other token, parsing as atom")
          withComments(parseAtom)(state).flatMap { case (next, afterNext) =>
            val updatedTerms = Vector(expr, next)
            debug(s"REST: After parsing other token as atom, terms: $updatedTerms")
            parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
              debug(s"REST: After recursive parseRest with other token, result=$result")
              result match {
                case opSeq: OpSeq =>
                  debug(s"REST: Result is OpSeq, combining with expr")
                  (OpSeq(Vector(expr) ++ opSeq.seq, None), finalState)
                case _ =>
                  debug(s"REST: Result is not OpSeq, using updatedTerms")
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }

        // Error handling
        case Left(error) =>
          debug(s"REST: Got error: $error")
          Left(error)
      }
    }
  }

  // Handle block arguments
  def handleBlockArgument(expr: Expr, state: LexerState)(
      leadingComments: Vector[Comment]
  ): Either[ParseError, (Expr, LexerState)] = {
    debug(s"BLOCK_ARG: Starting with expr=$expr, state=$state")
    
    withComments(parseBlock)(state).flatMap { case (block, afterBlock) =>
      debug(s"BLOCK_ARG: Parsed block=$block, afterBlock=$afterBlock")
      
      expr match {
        case opSeq: OpSeq =>
          // Handle case where the expression is already an OpSeq (e.g., "a op b { block }")
          debug("BLOCK_ARG: OpSeq with block")
          val updatedSeq = opSeq.seq :+ block
          debug(s"BLOCK_ARG: Updated OpSeq: $updatedSeq")
          Right((OpSeq(updatedSeq, None), afterBlock))

        case funcCall: FunctionCall =>
          // Handle function call with block argument
          debug("BLOCK_ARG: Adding block as argument to existing function call")
          val newCall = FunctionCall(
            funcCall,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(state.sourcePos)), Some(afterBlock.sourcePos))
          )
          debug(s"BLOCK_ARG: Created function call with block: $newCall")
          Right((newCall, afterBlock))
          
        case id: ConcreteIdentifier =>
          // Handle identifier with block
          debug("BLOCK_ARG: Creating function call with block argument from identifier")
          val functionCall = FunctionCall(
            id,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(state.sourcePos)), Some(afterBlock.sourcePos))
          )
          debug(s"BLOCK_ARG: Created function call: $functionCall")
          Right((functionCall, afterBlock))
          
        case _ =>
          // Default case
          debug("BLOCK_ARG: Default handling for block")
          Right((block, afterBlock))
      }
    }
  }

  // Colon handling
  def handleColon(sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[Comment]
  ): Either[ParseError, (Expr, LexerState)] = {
    val afterColon = state.advance()
    val updatedTerms = terms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
    debug(s"parseRest: After adding colon, terms: $updatedTerms")

    withComments(parseAtom)(afterColon).flatMap { case (next, afterNext) =>
      val newTerms = updatedTerms :+ next
      debug(s"parseRest: After parsing atom after colon, terms: $newTerms")

      parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
        result match {
          case opSeq: OpSeq =>
            (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
          case _ =>
            (OpSeq(newTerms, None), finalState)
        }
      }
    }
  }

  // Main parsing methods
  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Collect leading comments and initialize terms vector
    val (leadingComments, current) = collectComments(state)
    debug(s"Starting parseExpr with state: $current")

    // Main parsing logic - handle different token types
    current.current match {
      // Prefix operator
      case Right(Token.Operator(op, sourcePos)) =>
        debug(s"parseExpr: Starting with operator $op")
        val afterOp = current.advance()
        afterOp.current match {
          // Function call form: op(args)
          case Right(Token.LParen(_)) =>
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
          // Prefix form: op expr
          case _ =>
            debug("parseExpr: Parsing atom after initial operator")
            val opId = ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))
            withComments(parseAtom)(afterOp).flatMap { case (expr, afterExpr) =>
              val terms = Vector(opId, expr)
              debug(s"parseExpr: After initial operator and atom, terms: $terms")

              if (afterExpr.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right((OpSeq(terms, None), afterExpr))
              } else {
                parseRest(expr, afterExpr)(leadingComments)
              }
            }
        }

      // Treat all identifiers uniformly, handling them in the parseRest process
      case Right(Token.Identifier(parts, sourcePos)) =>
        debug(s"parseExpr: Starting with identifier ${charsToString(parts)}")
        val text = charsToString(parts)
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val afterId = current.advance()
        
        // Handle the token after the identifier
        afterId.current match {
          case Right(Token.LParen(_)) =>
            // Function call
            debug("parseExpr: Found lparen after identifier, parsing function call")
            parseTuple(afterId).flatMap { case (tuple, afterTuple) =>
              val functionCall = FunctionCall(id, tuple, createMeta(Some(sourcePos), Some(sourcePos)))
              parseRest(functionCall, afterTuple)(leadingComments)
            }
            
          case Right(Token.LBrace(_)) =>
            // Block after identifier
            debug("parseExpr: Found lbrace after identifier, parsing block")
            withComments(parseBlock)(afterId).flatMap { case (block, afterBlock) =>
              val terms = Vector(id, block)
              parseRest(block, afterBlock)(leadingComments).map { case (result, finalState) =>
                result match {
                  case opSeq: OpSeq => (OpSeq(Vector(id) ++ opSeq.seq, None), finalState)
                  case _ => (OpSeq(terms, None), finalState)
                }
              }
            }
            
          case Right(Token.Operator(op, opSourcePos)) =>
            // Operator after identifier (e.g., 'val a = 5', 'case x => y')
            debug(s"parseExpr: Found operator $op after identifier")
            val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
            val afterOp = afterId.advance()
            
            withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
              val terms = Vector(id, opId, next)
              debug(s"parseExpr: After parsing sequence: $id $op $next")
              
              parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
                result match {
                  case opSeq: OpSeq => (OpSeq(Vector(id, opId) ++ opSeq.seq, None), finalState)
                  case _ => (OpSeq(terms, None), finalState)
                }
              }
            }
            
          case _ =>
            // Just a bare identifier
            debug(s"parseExpr: Bare identifier $text")
            parseRest(id, afterId)(leadingComments)
        }

      // Standard expression handling
      case _ =>
        debug("parseExpr: Starting with atom")
        withComments(parseAtom)(current).flatMap { case (first, afterFirst) =>
          debug(s"parseExpr: After initial atom, got: $first")
          parseRest(first, afterFirst)(leadingComments)
        }
    }
  }

  /** Checks for the pattern of a right brace followed by a newline. This is used to detect block termination in certain contexts.
    */
  private def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
    // First check the preconditions - must be in context where newlines are significant,
    // and previous token must be a closing brace
    if (!state.newLineAfterBlockMeansEnds || !state.previousToken.exists(_.isInstanceOf[Token.RBrace])) {
      return false
    }

    // Check if current token contains a newline or is EOF
    val hasNewline = state.current match {
      case Right(ws: Token.Whitespace) =>
        lazy val wsContent = for {
          source <- sourceOffset.readContent.toOption
          range = ws.sourcePos.range
          if range.start.index.utf16 < source.length && range.end.index.utf16 <= source.length
          content = source.substring(range.start.index.utf16, range.end.index.utf16)
        } yield content

        wsContent.exists(_.contains('\n'))
      case Right(_: Token.EOF)       => true
      case Right(_: Token.Semicolon) => true // Also treat semicolons as terminators
      case _                                                                   => false
    }

    // Log if pattern is detected and debug is enabled
    if (DEBUG && hasNewline) debug("}\n check: pattern detected")
    hasNewline
  }

  private def handleDotCall(dotSourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
    val afterDot = state.advance() // Skip the dot
    afterDot.current match {
      case Right(Token.Identifier(parts, idSourcePos1)) =>
        val afterId = afterDot.advance()
        val field = createIdentifier(parts, idSourcePos1)
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(state: LexerState): Either[ParseError, (Expr, LexerState)] =
          state.current match {
            case Right(Token.LParen(_)) =>
              parseTuple(state).flatMap { case (args, afterArgs) =>
                telescope = telescope :+ args
                parseNextTelescope(afterArgs)
              }
            case Right(Token.LBrace(_)) =>
              parseBlock(state).flatMap { case (block, afterBlock) =>
                telescope = telescope :+ Tuple(Vector(block), None)
                parseNextTelescope(afterBlock)
              }
            case Right(Token.Dot(nextDotSourcePos)) =>
              val dotCall = createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(state.sourcePos))
              handleDotCall(nextDotSourcePos, state, Vector(dotCall))
            case _ =>
              Right((createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(state.sourcePos)), state))
          }

        parseNextTelescope(afterId)
      case Right(Token.Operator(op, idSourcePos)) =>
        val afterOp = afterDot.advance()
        val field = ConcreteIdentifier(op, createMeta(Some(idSourcePos), Some(idSourcePos)))
        afterOp.current match {
          case Right(Token.LParen(_)) =>
            parseTuple(afterOp).map { case (args, afterArgs) =>
              (createDotCall(terms.last, field, Vector(args), Some(dotSourcePos), Some(afterArgs.sourcePos)), afterArgs)
            }
          case _ =>
            Right((createDotCall(terms.last, field, Vector.empty, Some(dotSourcePos), Some(idSourcePos)), afterOp))
        }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseAtom(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    debug(s"ATOM: Starting parseAtom with state: $state, token: ${state.current}")
    state.current match {
      // Block parsing - explicitly handle blocks as atoms
      case Right(Token.LBrace(_)) =>
        debug("ATOM: Found opening brace, parsing block")
        withComments(parseBlock)(state)
        
      // Object parsing (starts with opening brace like blocks)
      case LBrace(_) =>
        // Check for empty object or block
        val afterLBrace = state.advance()
        debug(s"ATOM: After LBrace, token: ${afterLBrace.current}")
        afterLBrace.current match {
          case Left(err) => 
            debug(s"ATOM: Error after LBrace: $err")
            Left(err)
          case RBrace(_) => 
            debug("ATOM: Empty object")
            parseObject(state) // Empty object
          case Id(_, _) | Sym(_, _) | Str(_, _) =>
            // Look ahead for any operator to determine if it's an object
            val afterId = state.advance().advance()
            debug(s"ATOM: After Id/Sym/Str, token: ${afterId.current}")
            afterId.current match {
              case Left(err) => 
                debug(s"ATOM: Error after Id/Sym/Str: $err")
                Left(err)
              case Op(_, _) => 
                debug("ATOM: Found operator after Id/Sym/Str, parsing as object")
                parseObject(state) // Any operator indicates object
              case _ => 
                debug("ATOM: No operator after Id/Sym/Str, parsing as block")
                withComments(parseBlock)(state)
            }
          case _ => 
            debug(s"ATOM: Other token after LBrace: ${afterLBrace.current}, parsing as block")
            withComments(parseBlock)(state)
        }

      case LParen(_) => 
        debug("ATOM: Found left paren, parsing as tuple")
        parseTuple(state)

      case Id(parts, sourcePos) =>
        val idStr = charsToString(parts)
        debug(s"ATOM: Found identifier: $idStr")
        val afterId = state.advance()
        afterId.current match {
          case LBracket(_) =>
            debug("ATOM: Found left bracket after identifier, parsing as generic type params")
            // Generic type parameters
            val identifier = createIdentifier(parts, sourcePos)
            withComments(parseList)(afterId).flatMap { case (typeParams, afterTypeParams) =>
              afterTypeParams.current match {
                case LParen(_) =>
                  debug("ATOM: Found left paren after generic type params, parsing as function call")
                  // Function call with generic type args
                  parseTuple(afterTypeParams).map { case (tuple, afterArgs) =>
                    val typeParamsList: ListExpr = typeParams
                    val typeCall = createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos))
                    (createFunctionCall(typeCall, tuple, Some(sourcePos), Some(afterArgs.sourcePos)), afterArgs)
                  }
                case _ =>
                  debug("ATOM: No left paren after generic type params, returning generic type")
                  // Just the generic type parameters
                  val typeParamsList: ListExpr = typeParams
                  Right(
                    (createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos)), afterTypeParams)
                  )
              }
            }
          case LParen(_) =>
            debug("ATOM: Found left paren after identifier, parsing as function call")
            // Regular function call
            val identifier = createIdentifier(parts, sourcePos)
            parseFunctionCallWithId(identifier, afterId)
          case _ =>
            debug(s"ATOM: Found bare identifier: $idStr, returning as ConcreteIdentifier")
            // Plain identifier
            Right((createIdentifier(parts, sourcePos), afterId))
        }

      case Int(value, pos) =>
        debug(s"ATOM: Found integer: $value")
        parseInt(state)
      case Rat(_, _) => 
        debug("ATOM: Found rational number")
        parseRational(state)
      case Str(value, pos) =>
        debug(s"ATOM: Found string: ${charsToString(value)}")
        val stringLiteral = ConcreteStringLiteral(charsToString(value), createMeta(Some(pos), Some(pos)))
        Right((stringLiteral, state.advance()))
      case Sym(_, _) => 
        debug("ATOM: Found symbol")
        parseSymbol(state)

      case LBracket(_) => 
        debug("ATOM: Found left bracket, parsing as list")
        withComments(parseList)(state)

      case Right(token) => 
        debug(s"ATOM: Unexpected token: $token")
        Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))

      case Err(error) => 
        debug(s"ATOM: Error: $error")
        Left(error)
    }
  }

  // Helper method to check if a token is a terminator (right delimiter or comma/semicolon)
  private def isTerminator(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket | _: Token.Comma | _: Token.Semicolon | _: Token.EOF => true
    case _                                                                                                          => false
  }

  // Helper to check if current state has a terminator
  private def isAtTerminator(state: LexerState): Boolean = state.current match {
    case Right(token) => isTerminator(token)
    case _            => false
  }

  // Helper method to check if a token is specifically a right delimiter
  private def isRightDelimiter(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket => true
    case _                                                     => false
  }

  def parseExprList(state: LexerState): Either[ParseError, (Vector[Expr], LexerState)] = {
    // Replace skipComments with collectComments to preserve comments
    val (_leadingListComments, initialState) = collectComments(state)

    @tailrec
    def parseElements(current: LexerState, exprs: Vector[Expr], maxExprs: Int): Either[ParseError, (Vector[Expr], LexerState)] =
      if (exprs.length >= maxExprs) {
        Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", state.sourcePos.range.start))
      } else {
        debug(s"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
        current.current match {
          case Right(token) if isRightDelimiter(token) =>
            debug("Found right delimiter after expression")
            Right((exprs, current))
          case Right(_: Token.Comment | _: Token.Whitespace) =>
            // Collect comments instead of skipping
            val (_, afterComments) = collectComments(current)
            parseElements(afterComments, exprs, maxExprs)
          case Right(_: Token.Comma | _: Token.Semicolon) =>
            debug("Found comma or semicolon, skipping")
            // Collect any comments after comma/semicolon
            val (_, afterDelimiter) = collectComments(current.advance())
            parseElements(afterDelimiter, exprs, maxExprs)
          case _ =>
            debug("Parsing expression")
            parseExpr(current) match {
              case Left(err)                => Left(err)
              case Right((expr, afterExpr)) =>
                // Collect comments after the expression
                val (trailingComments, afterComments) = collectComments(afterExpr)

                // Check if we've reached a terminator
                afterComments.current match {
                  case Right(token) if isRightDelimiter(token) =>
                    // Attach trailing comments to the expression if any
                    val updatedExpr = if (trailingComments.nonEmpty) {
                      expr.updateMeta { meta =>
                        val newMeta = createMetaWithComments(
                          meta.flatMap(_.sourcePos),
                          Vector.empty,
                          trailingComments
                        )
                        // Merge with existing meta
                        mergeMeta(meta, newMeta)
                      }
                    } else {
                      expr
                    }
                    Right((exprs :+ updatedExpr, afterComments))

                  case Right(_: Token.Comma | _: Token.Semicolon) =>
                    debug("Found comma or semicolon after expression")
                    // Attach trailing comments to the expression if any
                    val updatedExpr = if (trailingComments.nonEmpty) {
                      expr.updateMeta { meta =>
                        val newMeta = createMetaWithComments(
                          meta.flatMap(_.sourcePos),
                          Vector.empty,
                          trailingComments
                        )
                        // Merge with existing meta
                        mergeMeta(meta, newMeta)
                      }
                    } else {
                      expr
                    }
                    val (_, afterDelimiter) = collectComments(afterComments.advance())
                    parseElements(afterDelimiter, exprs :+ updatedExpr, maxExprs)

                  case _ =>
                    // We haven't reached a terminator, treat this as a parsing error
                    Left(ParseError("Expected delimiter after expression", afterComments.sourcePos.range.start))
                }
            }
        }
      }

    parseElements(initialState, Vector.empty, LexerV2.MAX_LIST_ELEMENTS)
  }

  private def parseTuple(state: LexerState): Either[ParseError, (Tuple, LexerState)] = state.current match {
    case LParen(sourcePos) =>
      val (leadingComments, afterLParen) = collectComments(state.advance())
      for {
        (exprs, afterExprs) <- parseExprList(afterLParen)
        (trailingComments, afterList) = collectComments(afterExprs)
        result <- afterList.current match {
          case RParen(_) =>
            val meta =
              if leadingComments.nonEmpty || trailingComments.nonEmpty then
                createMeta(Some(sourcePos), Some(afterList.sourcePos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments, trailingComments)))
              else createMeta(Some(sourcePos), Some(afterList.sourcePos))
            Right((Tuple(exprs, meta), afterList.advance()))
          case _ => Left(expectedError("right parenthesis", afterList.current))
        }
      } yield result
    case _ => Left(expectedError("left parenthesis", state.current))
  }

  private def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    // Enable newLineAfterBlockMeansEnds for all blocks
    val contextState = state.withNewLineTermination(true)
    debug(s"parseBlock: starting with state=$contextState")

    val (_, current) = collectComments(contextState)
    var statements = Vector[Expr]()
    var result: Option[Expr] = None
    var maxExpressions = 100 // Prevent infinite loops

    // Skip the opening brace
    current.current match {
      case Right(Token.LBrace(_)) =>
        debug("parseBlock: Found opening brace")
        val (_blockStartComments, afterBrace) = collectComments(current.advance())

        var blockCurrent = afterBrace
        debug(s"parseBlock: After opening brace, blockCurrent=$blockCurrent")

        // Regular block parsing - all statements are treated the same
        while (maxExpressions > 0) {
          maxExpressions -= 1

          val (blockComments, withoutComments) = collectComments(blockCurrent)
          blockCurrent = withoutComments

          blockCurrent.current match {
            case Right(Token.RBrace(_)) =>
              debug(s"parseBlock: Found closing brace, statements=${statements.size}, has result=${result.isDefined}")
              val finalBlock = Block(statements, result, None)
              blockCurrent = blockCurrent.advance()
              debug(s"parseBlock: Returning block with ${statements.size} statements, result=${result.isDefined}")
              return Right((finalBlock, blockCurrent))

            case Right(Token.Semicolon(_)) =>
              debug("parseBlock: Found semicolon, advancing")
              val (_, afterSemi) = collectComments(blockCurrent.advance())
              blockCurrent = afterSemi
              debug(s"parseBlock: After semicolon, blockCurrent=$blockCurrent")

            case Right(Token.Whitespace(_, _)) =>
              debug("parseBlock: Found whitespace, advancing")
              val (_, afterWs) = collectComments(blockCurrent.advance())
              blockCurrent = afterWs
              debug(s"parseBlock: After whitespace, blockCurrent=$blockCurrent")

            case _ =>
              debug(s"parseBlock: Parsing expression at token ${blockCurrent.current}")
              parseExpr(blockCurrent) match {
                case Left(err) =>
                  debug(s"parseBlock: Error parsing expression: $err")
                  return Left(err)
                case Right((expr, next)) =>
                  debug(s"parseBlock: Parsed expression: $expr, next token: ${next.current}")

                  next.current match {
                    case Right(Token.RBrace(_)) =>
                      debug("parseBlock: Expression followed by closing brace, setting as result")
                      // This is the V1 style: put the last expression in the result field
                      result = Some(expr)
                      blockCurrent = next.advance()
                      debug(s"parseBlock: Returning block with ${statements.size} statements and result=$expr")
                      return Right((Block(statements, result, None), blockCurrent))

                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Expression followed by semicolon, adding to statements")
                      statements = statements :+ expr

                      val (_, afterSemi) = collectComments(next.advance())
                      blockCurrent = afterSemi
                      debug(s"parseBlock: After semicolon, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(whitespaceTok @ Token.Whitespace(_, _)) =>
                      debug("parseBlock: Expression followed by whitespace, checking for newlines")

                      // Get next token after whitespace
                      val (_, afterWs) = collectComments(next)

                      // Add statement and advance
                      statements = statements :+ expr
                      blockCurrent = afterWs
                      debug(s"parseBlock: After whitespace, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(t) =>
                      debug(s"parseBlock: Unexpected token after expression: $t")
                      return Left(ParseError("Expected ';', whitespace, or '}' after expression in block", t.sourcePos.range.start))

                    case Left(err) =>
                      debug(s"parseBlock: Error after expression: $err")
                      return Left(err)
                  }
              }
          }
        }

        debug("parseBlock: Too many expressions in block")
        Left(ParseError("Too many expressions in block", current.sourcePos.range.start))
      case Right(t) =>
        debug(s"parseBlock: Expected '{' but found $t")
        Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) =>
        debug(s"parseBlock: Error at start of block: $err")
        Left(err)
    }
  }

  // Helper method to check if whitespace contains a newline
  private def isNewlineWhitespace(token: Token.Whitespace): Boolean = {
    val maybeSource = sourceOffset.readContent.toOption
    maybeSource.exists { source =>
      val startPos = token.sourcePos.range.start.index.utf16
      val endPos = token.sourcePos.range.end.index.utf16
      if (startPos < source.length && endPos <= source.length) {
        val whitespaceText = source.substring(startPos, endPos)
        whitespaceText.contains('\n')
      } else {
        false
      }
    }
  }

  private def parseObject(initialState: LexerState): Either[ParseError, (ObjectExpr, LexerState)] = {
    // Collect comments before the object
    val (leadingComments, current) = collectComments(initialState)

    current.current match {
      case Right(Token.LBrace(sourcePos)) =>
        // Collect comments after the opening brace
        val (afterBraceComments, afterBrace) = collectComments(current.advance())

        def parseFields(state: LexerState, clauses: Vector[ObjectClause]): Either[ParseError, (Vector[ObjectClause], LexerState)] =
          state.current match {
            case Right(Token.RBrace(_)) =>
              Right((clauses, state))
            case Right(Token.Identifier(parts, idSourcePos)) =>
              val identifier = ConcreteIdentifier(charsToString(parts), createMeta(Some(idSourcePos), Some(idSourcePos)))
              parseField(state.advance(), identifier, idSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap(nextStateAfterComma => parseFields(nextStateAfterComma, clauses :+ clause))
              }
            case Right(Token.StringLiteral(chars, strSourcePos)) =>
              val stringLiteral = ConcreteStringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
              parseField(state.advance(), stringLiteral, strSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap(nextStateAfterComma => parseFields(nextStateAfterComma, clauses :+ clause))
              }
            case Right(Token.SymbolLiteral(value, symSourcePos)) =>
              val symbolLiteral = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
              parseField(state.advance(), symbolLiteral, symSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap(nextStateAfterComma => parseFields(nextStateAfterComma, clauses :+ clause))
              }
            case Right(t) =>
              Left(ParseError("Expected identifier, string literal, symbol literal or '}' in object", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }

        def parseField(state: LexerState, key: Expr, keySourcePos: SourcePos): Either[ParseError, (ObjectClause, LexerState)] =
          state.current match {
            case Right(Token.Operator(op, _)) =>
              val afterOp = state.advance()
              parseExpr(afterOp).flatMap { case (value, afterValue) =>
                // Handle operators for object fields appropriately
                debug(s"parseField: Handling operator '$op'")
                
                // Special case for => operator - creates ObjectExprClauseOnValue
                if (op == "=>") {
                  debug("parseField: Creating ObjectExprClauseOnValue for => operator")
                  Right((ObjectExprClauseOnValue(key, value), afterValue))
                } else {
                  // For other operators like =, handle string literals and identifiers
                  key match {
                    case stringLit: ConcreteStringLiteral =>
                      val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                      Right((ObjectExprClause(idKey, value), afterValue))
                    case qualifiedName: QualifiedName =>
                      Right((ObjectExprClause(qualifiedName, value), afterValue))
                    case id: ConcreteIdentifier =>
                      Right((ObjectExprClause(id, value), afterValue))
                    case other =>
                      Left(ParseError(s"Expected identifier or string for object field key but got: $other", keySourcePos.range.start))
                  }
                }
              }
            case Right(t) =>
              Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }

        def checkAfterField(state: LexerState): Either[ParseError, LexerState] =
          state.current match {
            case Right(Token.Comma(_)) =>
              // Collect comments after comma
              val (_, afterComma) = collectComments(state.advance())
              Right(afterComma)
            case Right(Token.RBrace(_)) =>
              Right(state)
            case Right(t) =>
              Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }

        parseFields(afterBrace, Vector.empty).flatMap { case (clauses, finalState) =>
          finalState.current match {
            case Right(Token.RBrace(endPos)) =>
              // Create meta with comments
              val objectMeta = if (leadingComments.nonEmpty || afterBraceComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBraceComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }

              Right((ObjectExpr(clauses, objectMeta), finalState.advance()))
            case Right(t) =>
              Left(ParseError("Expected '}' at end of object", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }
        }
      case Right(t) =>
        Left(ParseError("Expected '{' at start of object", t.sourcePos.range.start))
      case Left(err) =>
        Left(err)
    }
  }

  private def parseList(state: LexerState): Either[ParseError, (ListExpr, LexerState)] = {
    val (leadingComments, initialState) = collectComments(state)

    initialState.current match {
      case LBracket(sourcePos) =>
        val (afterBracketComments, afterBracket) = collectComments(initialState.advance())

        @tailrec
        def parseElements(current: LexerState, exprs: Vector[Expr]): Either[ParseError, (Vector[Expr], LexerState)] =
          if (exprs.length >= LexerV2.MAX_LIST_ELEMENTS) {
            Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", sourcePos.range.start))
          } else
            current.current match {
              case RBracket(_) => Right((exprs, current))
              case Comma(_) =>
                val (_, afterComma) = collectComments(current.advance())
                parseElements(afterComma, exprs)
              case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_, _)) =>
                val (_, afterComments) = collectComments(current)
                parseElements(afterComments, exprs)
              case _ =>
                parseExpr(current) match {
                  case Left(err) => Left(err)
                  case Right((expr, afterExpr)) =>
                    val (_, afterComments) = collectComments(afterExpr)
                    afterComments.current match {
                      case RBracket(_) => Right((exprs :+ expr, afterComments))
                      case Comma(_) =>
                        val (_, afterComma) = collectComments(afterComments.advance())
                        parseElements(afterComma, exprs :+ expr)
                      case _ => Left(expectedError("',' or ']' in list", afterComments.current))
                    }
                }
            }

        parseElements(afterBracket, Vector.empty).flatMap { case (exprs, finalState) =>
          finalState.current match {
            case RBracket(endPos) =>
              val listMeta = if (leadingComments.nonEmpty || afterBracketComments.nonEmpty) {
                createMeta(Some(sourcePos), Some(endPos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBracketComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }
              Right((ListExpr(exprs, listMeta), finalState.advance()))
            case _ => Left(expectedError("']' at end of list", finalState.current))
          }
        }
      case _ => Left(expectedError("[", initialState.current))
    }
  }

  /** Collects comments from the current state. Returns a tuple of (collected comments, updated state).
    */
  private def collectComments(state: LexerState): (Vector[Comment], LexerState) = {
    @tailrec
    def collectRec(current: LexerState, comments: Vector[Comment]): (Vector[Comment], LexerState) =
      if (!current.isAtEnd && current.current.exists {
            case _: Token.Comment | _: Token.Whitespace => true
            case _ => false
          }) {
        current.current match {
          case Right(Token.Comment(text, sourcePos)) =>
            val commentType = if (text.trim.startsWith("//")) {
              CommentType.OneLine
            } else {
              CommentType.MultiLine
            }

            val comment = Comment(
              content = text.trim,
              typ = commentType,
              sourcePos = Some(sourcePos)
            )
            collectRec(current.advance(), comments :+ comment)
          case Right(_: Token.Whitespace) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just advance the token - we'll hit another token eventually
            collectRec(current.advance(), comments)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }

    collectRec(state, Vector.empty)
  }

  /** Collects trailing comments after an expression until a newline or non-comment token.
    */
  private def collectTrailingComments(state: LexerState): (Vector[Comment], LexerState) = {
    // For trailing comments, we only collect comments that appear on the same line
    // (until we hit a newline in whitespace)
    @tailrec
    def collectRec(
        current: LexerState,
        comments: Vector[Comment],
        hitNewline: Boolean
    ): (Vector[Comment], LexerState) =
      if (
        !current.isAtEnd && !hitNewline &&
        current.current.exists {
          case _: Token.Comment | _: Token.Whitespace => true
          case _ => false
        }
      ) {
        current.current match {
          case Right(Token.Comment(text, sourcePos)) =>
            val commentType = if (text.trim.startsWith("//")) {
              CommentType.OneLine
            } else {
              CommentType.MultiLine
            }

            val comment = Comment(
              content = text.trim,
              typ = commentType,
              sourcePos = Some(sourcePos)
            )
            collectRec(current.advance(), comments :+ comment, hitNewline)
          case Right(_: Token.Whitespace) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just assume any whitespace might contain a newline and stop collecting
            collectRec(current.advance(), comments, true)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }

    collectRec(state, Vector.empty, false)
  }

  /** Creates ExprMeta with comments.
    */
  private def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[Comment] = Vector.empty,
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[ExprMeta] =
    Option.when(sourcePos.nonEmpty || leadingComments.nonEmpty || trailingComments.nonEmpty) {
      ExprMeta(sourcePos, createCommentInfo(leadingComments, trailingComments))
    }

  // Helper for creating function calls
  private def createFunctionCall(
      func: Expr,
      args: Expr,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): FunctionCall =
    FunctionCall(
      func,
      args.asInstanceOf[Tuple],
      createMeta(startSourcePos, endSourcePos)
    )

  // Special version for type parameters
  private def createFunctionCallWithTypeParams(
      func: Expr,
      typeParams: ListExpr,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): FunctionCall =
    FunctionCall(
      func,
      typeParams,
      createMeta(startSourcePos, endSourcePos)
    )

  // Parse a function call with the given identifier
  private def parseFunctionCallWithId(
      identifier: ConcreteIdentifier,
      state: LexerState
  ): Either[ParseError, (FunctionCall, LexerState)] =
    state.current match {
      case LParen(_) =>
        parseTuple(state).map { case (args, afterArgs) =>
          val funcSourcePos = identifier.meta.flatMap(_.sourcePos)
          (createFunctionCall(identifier, args, funcSourcePos, Some(afterArgs.sourcePos)), afterArgs)
        }
      case _ =>
        Left(ParseError("Expected left parenthesis for function call", state.sourcePos.range.start))
    }

  /** Generic parser combinator that adds comment handling to any parse method */
  private def withComments[T <: Expr](
      parseMethod: LexerState => Either[ParseError, (T, LexerState)]
  )(state: LexerState): Either[ParseError, (T, LexerState)] = {
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments(state)

    // Parse the expression using the provided method
    parseMethod(afterLeadingComments).flatMap { case (expr, afterExpr) =>
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
          mergeMeta(existingMeta, newMeta)
        }
      } else {
        expr
      }

      // Cast is safe because we're only modifying metadata
      Right((updatedExpr.asInstanceOf[T], finalState))
    }
  }

  private def createCommentInfo(
      leadingComments: Vector[Comment],
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[CommentInfo] =
    Option.when(leadingComments.nonEmpty || trailingComments.nonEmpty) {
      CommentInfo(
        commentBefore = leadingComments,
        commentInBegin = Vector.empty,
        commentInEnd = Vector.empty,
        commentEndInThisLine = trailingComments
      )
    }

  // Helper for handling common error patterns
  private def withErrorHandling[T](
      parser: LexerState => Either[ParseError, (T, LexerState)],
      errorMsg: String
  ): LexerState => Either[ParseError, (T, LexerState)] = state =>
    parser(state) match {
      case Left(err) => Left(ParseError(s"$errorMsg: ${err.message}", err.pos))
      case right     => right
    }

  // Helper that combines collecting comments and parsing with error handling
  private def withCommentsAndErrorHandling[T <: Expr](
      parser: LexerState => Either[ParseError, (T, LexerState)],
      errorMsg: String
  ): LexerState => Either[ParseError, (T, LexerState)] = state => {
    val (leadingComments, afterLeadingComments) = collectComments(state)

    withErrorHandling(parser, errorMsg)(afterLeadingComments).flatMap { case (expr, afterExpr) =>
      val (trailingComments, finalState) = collectTrailingComments(afterExpr)

      // Update expression with comments if needed
      val updatedExpr = if (leadingComments.nonEmpty || trailingComments.nonEmpty) {
        expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )
          mergeMeta(existingMeta, newMeta)
        }
      } else {
        expr
      }

      Right((updatedExpr.asInstanceOf[T], finalState))
    }
  }

  private def parseString(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            PartialFunction.condOpt(token) { case Token.StringLiteral(value, sourcePos) =>
              (charsToString(value), sourcePos)
            },
          (value, meta) => ConcreteStringLiteral(value, meta),
          "Expected string literal"
        ),
      "Error parsing string"
    )(state)

  // Create a helper method for parsing literals with common pattern
  private def parseLiteral[T <: Expr](
      state: LexerState,
      extract: Token => Option[(String, SourcePos)],
      create: (String, Option[ExprMeta]) => T,
      errorMsg: String
  ): Either[ParseError, (T, LexerState)] =
    state.current match {
      case Right(token) =>
        extract(token) match {
          case Some((value, sourcePos)) =>
            val meta = createMeta(Some(sourcePos), Some(sourcePos))
            Right((create(value, meta), state.advance()))
          case None =>
            Left(ParseError(errorMsg, state.sourcePos.range.start))
        }
      case Left(err) => Left(err)
    }

  // Helper functions for other literal types
  def parseInt(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          {
            case Token.IntegerLiteral(value, sourcePos) =>
              // Handle different bases
              val (numStr, base) =
                if (value.startsWith("0x")) (value.drop(2), 16)
                else if (value.startsWith("0b")) (value.drop(2), 2)
                else (value, 10)
              try
                Some((BigInt(numStr, base).toString, sourcePos))
              catch {
                case _: NumberFormatException => None
              }
            case _ => None
          },
          (value, meta) => ConcreteIntegerLiteral(BigInt(value), meta),
          "Expected integer literal"
        ),
      "Error parsing integer"
    )(state)

  private def parseRational(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          {
            case Token.RationalLiteral(value, sourcePos) =>
              try
                Some((value, sourcePos))
              catch {
                case _: NumberFormatException => None
              }
            case _ => None
          },
          (value, meta) => ConcreteRationalLiteral(spire.math.Rational(BigDecimal(value)), meta),
          "Expected rational literal"
        ),
      "Error parsing rational number"
    )(state)

  private def parseSymbol(state: LexerState): Either[ParseError, (Expr, LexerState)] =
    withCommentsAndErrorHandling(
      st =>
        parseLiteral(
          st,
          token =>
            PartialFunction.condOpt(token) { case Token.SymbolLiteral(value, sourcePos) =>
              (value, sourcePos)
            },
          chester.syntax.concrete.SymbolLiteral.apply,
          "Expected symbol literal"
        ),
      "Error parsing symbol"
    )(state)

  // Helper to create identifier expressions
  private def createIdentifier(parts: Vector[StringChar], sourcePos: SourcePos): ConcreteIdentifier =
    ConcreteIdentifier(charsToString(parts), createMeta(Some(sourcePos), Some(sourcePos)))

  // Helper for creating field access expressions
  private def createDotCall(
      target: Expr,
      field: ConcreteIdentifier,
      args: Vector[Tuple] = Vector.empty,
      startSourcePos: Option[SourcePos],
      endSourcePos: Option[SourcePos]
  ): DotCall =
    DotCall(
      target,
      field,
      args,
      createMeta(startSourcePos, endSourcePos)
    )

}
