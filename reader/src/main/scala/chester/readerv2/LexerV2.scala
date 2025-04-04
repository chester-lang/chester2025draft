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

  // Helper for building operator sequences
  def buildOpSeq(terms: Vector[Expr])(state: LexerState, leadingComments: Vector[Comment]): Either[ParseError, Expr] = {
    debug(s"Building OpSeq with terms: $terms")
    terms match {
      case Vector() => Left(ParseError("Empty operator sequence", getStartPos(state.current)))
      case Vector(expr) if leadingComments.nonEmpty =>
        Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), leadingComments))))
      case Vector(expr) => Right(expr)
      case _            => Right(OpSeq(terms, Option.when(leadingComments.nonEmpty)(createMetaWithComments(None, leadingComments).get)))
    }
  }

  // Main expression continuation parser
  def parseRest(
      expr: Expr,
      state: LexerState
  )(leadingComments: Vector[Comment] = Vector.empty): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    var localTerms = Vector(expr)

    debug(s"parseRest called with expr: $expr, state: $current, current terms: $localTerms")

    // Special case for "so getthen { block }" - look ahead to check for the pattern
    if (expr.isInstanceOf[ConcreteIdentifier] && current.current.isRight) {
      val token = current.current.toOption.get
      if (token.isInstanceOf[Token.Whitespace]) {
        val afterWs = current.advance()
        if (afterWs.current.isRight) {
          val nextToken = afterWs.current.toOption.get
          if (nextToken.isInstanceOf[Token.Identifier]) {
            val idToken = nextToken.asInstanceOf[Token.Identifier]
            val afterId = afterWs.advance()
            
            // Check if the next token after the identifier is whitespace followed by a brace
            if (afterId.current.isRight && afterId.current.toOption.get.isInstanceOf[Token.Whitespace]) {
              val afterWs2 = afterId.advance()
              if (afterWs2.current.isRight && afterWs2.current.toOption.get.isInstanceOf[Token.LBrace]) {
                // This is the pattern: id whitespace id whitespace lbrace
                debug("parseRest: Detected infix expression with block pattern")
                
                // Add the second identifier to our terms
                val opText = idToken.parts.map(_.text).mkString
                val opId = ConcreteIdentifier(opText, createMeta(Some(idToken.sourcePos), Some(idToken.sourcePos)))
                localTerms = localTerms :+ opId
                
                // Parse the block
                return withComments(parseBlock)(afterWs2).map { case (block, afterBlock) =>
                  // Add the block to the terms
                  val finalTerms = localTerms :+ block
                  debug(s"parseRest: Created infix with block: $finalTerms")
                  
                  // Return the OpSeq with all terms
                  (OpSeq(finalTerms, None), afterBlock)
                }
              }
            }
          }
        }
      }
    }
    
    // Not our pattern, continue with normal parsing
    continueParseRest(expr, state, localTerms)(leadingComments)
  }
  
  // The original parseRest logic, now moved to a helper method
  def continueParseRest(
      expr: Expr,
      state: LexerState,
      localTerms: Vector[Expr]
  )(leadingComments: Vector[Comment] = Vector.empty): Either[ParseError, (Expr, LexerState)] = {
    var current = state
    
    // If we're at a terminator, return the expression directly
    if (current.isAtTerminator) {
      debug("parseRest: Hit terminator token")
      if (localTerms.size == 1) {
        // Return the expression directly
        Right((expr, current))
      } else {
        // Build an OpSeq
        debug(s"Building OpSeq with terms: $localTerms")
        // Need to wrap in the right return type
        buildOpSeq(localTerms)(state, leadingComments).map(result => (result, current))
      }
    } else {
      current.current match {
        // Match expression handling - treat like any other expression
        case Right(Token.Identifier(parts, _)) if parts.map(_.text).mkString == "match" && expr.isInstanceOf[ConcreteIdentifier] =>
          debug("parseRest: Found match keyword after identifier")
          val matchId = ConcreteIdentifier("match", createMeta(None, None))
          val afterMatch = current.advance()

          // For match blocks, parse using the regular block parser with no special case handling
          withComments(parseBlock)(afterMatch).map { case (block, afterBlock) =>
            // Create the match expression with the block as-is
            val matchExpr = OpSeq(Vector(expr, matchId, block), None)
            (matchExpr, afterBlock)
          }

        // Block argument handling
        case Right(Token.LBrace(braceSourcePos)) =>
          debug("parseRest: Found LBrace after expression, treating as block argument")
          handleBlockArgument(expr, state, localTerms, braceSourcePos)(leadingComments)

        // Colon handling (type annotations, etc)
        case Right(Token.Colon(sourcePos)) =>
          debug("parseRest: Found colon")
          handleColon(sourcePos, state, localTerms)(leadingComments)

        // Dot call handling
        case Right(Token.Dot(dotSourcePos)) =>
          debug("parseRest: Found dot")
          handleDotCall(dotSourcePos, current, localTerms).flatMap { case (dotCall, newState) =>
            val updatedTerms = Vector(dotCall)
            debug(s"parseRest: After dot call, terms: $updatedTerms")
            parseRest(dotCall, newState)(leadingComments)
          }

        // Operator handling
        case Right(Token.Operator(op, sourcePos)) =>
          debug(s"parseRest: Found operator $op")
          
          if (op == "=" && localTerms.size >= 2) {
            debug("parseRest: Found variable declaration or assignment")
            val afterOp = current.advance()
            withComments(parseExpr)(afterOp).map { case (valueExpr, afterValue) =>
              val updatedTerms = localTerms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ valueExpr
              (OpSeq(updatedTerms, None), afterValue)
            }
          } else if (op == "=>" && localTerms.size >= 2) {
            debug("parseRest: Found arrow pattern (pattern => body)")
            val afterOp = current.advance()
            withComments(parseExpr)(afterOp).map { case (bodyExpr, afterBody) =>
              val updatedTerms = localTerms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))) :+ bodyExpr
              (OpSeq(updatedTerms, None), afterBody)
            }
          } else {
            handleOperatorInRest(op, sourcePos, current, localTerms)(leadingComments)
          }

        // Identifier handling
        case Right(Token.Identifier(parts, sourcePos)) =>
          val text = charsToString(parts)
          debug(s"parseRest: Found identifier $text")
          handleIdentifierInRest(text, sourcePos, current, localTerms)(leadingComments)

        // Generic token handling
        case Right(_) =>
          debug("parseRest: Found other token, parsing as atom")
          withComments(parseAtom)(current).flatMap { case (next, afterNext) =>
            val updatedTerms = localTerms :+ next
            debug(s"parseRest: After parsing other token as atom, terms: $updatedTerms")
            parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }

        // Error handling
        case Left(error) =>
          debug(s"parseRest: Got error: $error")
          Left(error)
      }
    }
  }

  // Handle block arguments
  def handleBlockArgument(expr: Expr, state: LexerState, terms: Vector[Expr], braceSourcePos: SourcePos)(
      leadingComments: Vector[Comment]
  ): Either[ParseError, (Expr, LexerState)] = {
    debug(s"*** handleBlockArgument: Starting with expr=$expr, terms=$terms, state=$state")
    
    withComments(parseBlock)(state).flatMap { case (block, afterBlock) =>
      debug(s"*** handleBlockArgument: Parsed block=$block, afterBlock=$afterBlock")
      
      // The key distinction to determine if this is:
      // 1. A function call with block argument: f{block}
      // 2. An infix expression with block: a op {block}
      
      // For infix expressions in the form: so getthen { block }
      // "so" is in terms[0], "getthen" is the current expr
      // We need to check if we're in the parseRest phase after an identifier
      
      val isInfixWithBlock = terms.nonEmpty && 
                            terms.last.isInstanceOf[ConcreteIdentifier] &&
                            expr.isInstanceOf[ConcreteIdentifier] &&
                            !terms.contains(expr)
      
      debug(s"*** handleBlockArgument: isInfixWithBlock=$isInfixWithBlock, expr=$expr, terms=$terms")
      
      if (isInfixWithBlock) {
        debug("*** handleBlockArgument: Infix expression with block, adding block to operation sequence")
        // For infix expressions, include the operator and the block in the OpSeq
        val updatedTerms = terms :+ expr :+ block
        debug(s"*** handleBlockArgument: After adding block to infix, terms: $updatedTerms")
        
        // Create an OpSeq with all terms
        val result = OpSeq(updatedTerms, None)
        debug(s"*** handleBlockArgument: Created OpSeq: $result")
        
        // Return the result
        Right((result, afterBlock))
      } else {
        // Create appropriate expression based on context
        val newExpr = expr match {
          case funcCall: FunctionCall =>
            debug("*** handleBlockArgument: Adding block as argument to existing function call")
            FunctionCall(
              funcCall,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          case id: ConcreteIdentifier =>
            debug("*** handleBlockArgument: Creating function call with block argument from identifier")
            FunctionCall(
              id,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          case _ =>
            debug("*** handleBlockArgument: Default handling for block after expression")
            block
        }

        debug(s"*** handleBlockArgument: Created expression $newExpr")

        // For function calls, return directly
        if (newExpr.isInstanceOf[FunctionCall]) {
          debug("*** handleBlockArgument: Returning function call with block directly")
          Right((newExpr, afterBlock))
        } else {
          // For other expressions, update terms
          val updatedTerms = terms.dropRight(1) :+ newExpr
          debug(s"*** handleBlockArgument: Using updated terms: $updatedTerms")
          Right((OpSeq(updatedTerms, None), afterBlock))
        }
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

  // Operator handling
  def handleOperatorInRest(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[Comment]
  ): Either[ParseError, (Expr, LexerState)] = {
    val afterOp = state.advance()

    // Add operator to terms
    val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))

    // Create a regular OpSeq if we're at the end of a function call argument or similar boundary
    if (
      afterOp.current match {
        case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
        case _                                              => false
      }
    ) {
      debug(s"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
      buildOpSeq(updatedTerms)(state, leadingComments).map(result => (result, afterOp))
    } else {
      // Continue parsing the rest of the expression
      withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
        debug(s"parseRest: After parsing atom after operator, got: $next")
        val newTerms = updatedTerms :+ next
        debug(s"parseRest: Updated terms after operator: $newTerms")

        // Continue parsing the rest
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
  }

  // Identifier handling
  def handleIdentifierInRest(text: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[Comment]
  ): Either[ParseError, (Expr, LexerState)] = {
    val afterId = state.advance()

    afterId.current match {
      case Right(Token.LParen(_)) =>
        debug("parseRest: Found lparen after identifier")
        parseTuple(afterId).flatMap { case (tuple, afterTuple) =>
          val functionCall = FunctionCall(
            ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
            tuple,
            createMeta(Some(sourcePos), Some(sourcePos))
          )
          val updatedTerms = terms :+ functionCall
          debug(s"parseRest: After function call, terms: $updatedTerms")

          parseRest(functionCall, afterTuple)(leadingComments).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      case Right(Token.LBrace(_)) =>
        debug("parseRest: Found lbrace after identifier")
        withComments(parseBlock)(afterId).flatMap { case (block, afterBlock) =>
          // In V1 parser, a block after an identifier in infix is treated as part of the OpSeq
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val updatedTerms = terms :+ id :+ block
          debug(s"parseRest: After block in infix, terms: $updatedTerms")

          parseRest(block, afterBlock)(leadingComments).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      case Right(Token.Operator(op, opSourcePos)) =>
        debug(s"parseRest: Found operator $op after identifier")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
        val updatedTerms = terms :+ id :+ opId
        debug(s"parseRest: After adding id and op, terms: $updatedTerms")

        val afterOp = afterId.advance()
        withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
          val newTerms = updatedTerms :+ next
          debug(s"parseRest: After parsing atom after operator, terms: $newTerms")

          parseRest(next, afterNext)(leadingComments).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(newTerms, None), finalState)
            }
          }
        }
      case _ =>
        debug(s"parseRest: Found bare identifier $text")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val updatedTerms = terms :+ id
        debug(s"parseRest: After adding bare id, terms: $updatedTerms")

        parseRest(id, afterId)(leadingComments).map { case (result, finalState) =>
          result match {
            case opSeq: OpSeq =>
              (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
            case _ =>
              (OpSeq(updatedTerms, None), finalState)
          }
        }
    }
  }

  // Main parsing methods
  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    // Collect leading comments and initialize terms vector
    val (leadingComments, current) = collectComments(state)
    var terms = Vector.empty[Expr]
    debug(s"Starting parseExpr with state: $current")

    // Main parsing logic - handle different token types
    current.current match {
      // Special case for 'val' declarations
      case Right(Token.Identifier(parts, sourcePos)) if charsToString(parts) == "val" =>
        debug("parseExpr: Starting with 'val' keyword")
        terms = Vector(ConcreteIdentifier("val", createMeta(Some(sourcePos), Some(sourcePos))))
        val afterVal = current.advance()
        
        withComments(parseAtom)(afterVal).flatMap { case (nameExpr, afterName) =>
          terms = terms :+ nameExpr
          
          afterName.current match {
            case Right(Token.Operator(op, opSourcePos)) if op == "=" =>
              debug("parseExpr: Found = in val declaration")
              terms = terms :+ ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
              val afterEq = afterName.advance()
              
              withComments(parseExpr)(afterEq).map { case (valueExpr, afterValue) =>
                (OpSeq(terms :+ valueExpr, None), afterValue)
              }
            case _ => 
              Right((OpSeq(terms, None), afterName))
          }
        }
        
      // Special case for 'case' patterns
      case Right(Token.Identifier(parts, sourcePos)) if charsToString(parts) == "case" =>
        debug("parseExpr: Starting with 'case' keyword")
        terms = Vector(ConcreteIdentifier("case", createMeta(Some(sourcePos), Some(sourcePos))))
        val afterCase = current.advance()
        
        withComments(parseAtom)(afterCase).flatMap { case (patternExpr, afterPattern) =>
          terms = terms :+ patternExpr
          
          afterPattern.current match {
            case Right(Token.Operator(op, opSourcePos)) if op == "=>" =>
              debug("parseExpr: Found => in case pattern")
              terms = terms :+ ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
              val afterArrow = afterPattern.advance()
              
              withComments(parseExpr)(afterArrow).map { case (bodyExpr, afterBody) =>
                (OpSeq(terms :+ bodyExpr, None), afterBody)
              }
            case _ => 
              Right((OpSeq(terms, None), afterPattern))
          }
        }

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
            terms = Vector(ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))))
            withComments(parseAtom)(afterOp).flatMap { case (expr, afterExpr) =>
              terms = terms :+ expr
              debug(s"parseExpr: After initial operator and atom, terms: $terms")

              if (afterExpr.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right((OpSeq(terms, None), afterExpr))
              } else {
                parseRest(expr, afterExpr)(leadingComments)
              }
            }
        }

      // Keyword operator handling
      case Right(Token.Identifier(parts, sourcePos)) if strIsOperator(charsToString(parts)) =>
        debug(s"parseExpr: Starting with keyword operator ${charsToString(parts)}")
        val afterOp = current.advance()
        terms = Vector(ConcreteIdentifier(charsToString(parts), createMeta(Some(sourcePos), Some(sourcePos))))
        withComments(parseAtom)(afterOp).flatMap { case (expr, afterExpr) =>
          terms = terms :+ expr
          debug(s"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (afterExpr.isAtTerminator) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            Right((OpSeq(terms, None), afterExpr))
          } else {
            parseRest(expr, afterExpr)(leadingComments)
          }
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
      // For match expressions, always treat a case keyword as a terminator
      case Right(Token.Identifier(parts, _)) if parts.map(_.text).mkString == "case" => true
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

  private def parseAtom(current: LexerState): Either[ParseError, (Expr, LexerState)] =
    current.current match {
      case Left(err) => Left(err)
      case LBrace(_) =>
        // Check for empty object or block
        current.advance().current match {
          case Left(err)                        => Left(err)
          case RBrace(_)                        => parseObject(current) // Empty object
          case Id(_, _) | Sym(_, _) | Str(_, _) =>
            // Look ahead for = or => to determine if it's an object
            val afterId = current.advance().advance()
            afterId.current match {
              case Left(err)                            => Left(err)
              case Op(op, _) if op == "=" || op == "=>" => parseObject(current)
              case _                                    => withComments(parseBlock)(current)
            }
          case _ => withComments(parseBlock)(current)
        }

      case LParen(_) => parseTuple(current)

      case Id(parts, sourcePos) =>
        val afterId = current.advance()
        afterId.current match {
          case LBracket(_) =>
            // Generic type parameters
            val identifier = createIdentifier(parts, sourcePos)
            withComments(parseList)(afterId).flatMap { case (typeParams, afterTypeParams) =>
              afterTypeParams.current match {
                case LParen(_) =>
                  // Function call with generic type args
                  parseTuple(afterTypeParams).map { case (tuple, afterArgs) =>
                    val typeParamsList: ListExpr = typeParams
                    val typeCall = createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos))
                    (createFunctionCall(typeCall, tuple, Some(sourcePos), Some(afterArgs.sourcePos)), afterArgs)
                  }
                case _ =>
                  // Just the generic type parameters
                  val typeParamsList: ListExpr = typeParams
                  Right(
                    (createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos)), afterTypeParams)
                  )
              }
            }
          case LParen(_) =>
            // Regular function call
            val identifier = createIdentifier(parts, sourcePos)
            parseFunctionCallWithId(identifier, afterId)
          case _ =>
            // Plain identifier
            Right((createIdentifier(parts, sourcePos), afterId))
        }

      case Int(_, _) => parseInt(current)
      case Rat(_, _) => parseRational(current)
      case Str(value, pos) =>
        val stringLiteral = ConcreteStringLiteral(charsToString(value), createMeta(Some(pos), Some(pos)))
        Right((stringLiteral, current.advance()))
      case Sym(_, _) => parseSymbol(current)

      case LBracket(_) => withComments(parseList)(current)

      case Right(token) => Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))

      case Err(error) => Left(error)
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

  // Check if a token is a 'case' identifier
  private def isCaseIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(Token.Identifier(parts, _)) => parts.map(_.text).mkString == "case"
    case _                                 => false
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

            case _ if isCaseIdentifier(blockCurrent.current) =>
              // When we encounter 'case' at the beginning of an expression, we need to parse it
              // normally but ensure it's a separate statement
              debug("parseBlock: Found 'case' identifier, parsing statement")
              parseExpr(blockCurrent) match {
                case Left(err) =>
                  debug(s"parseBlock: Error parsing case expression: $err")
                  return Left(err)
                case Right((expr, next)) =>
                  debug(s"parseBlock: Parsed case statement: $expr")
                  statements = statements :+ expr

                  // Skip past semicolons
                  val afterExpr = next.current match {
                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Skipping semicolon after case statement")
                      next.advance()
                    case _ => next
                  }

                  blockCurrent = afterExpr
                  debug(s"parseBlock: Updated block state after case statement")
              }

            case _ =>
              debug(s"parseBlock: Parsing expression at token ${blockCurrent.current}")
              parseExpr(blockCurrent) match {
                case Left(err) =>
                  debug(s"parseBlock: Error parsing expression: $err")
                  return Left(err)
                case Right((expr, next)) =>
                  debug(s"parseBlock: Parsed expression: $expr, next token: ${next.current}")

                  // Check if the next token is a 'case' identifier
                  val nextTokenIsCase = isCaseIdentifier(next.current)

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

                      // Check for case after semicolon
                      val (_, afterSemi) = collectComments(next.advance())
                      if (isCaseIdentifier(afterSemi.current)) {
                        debug("parseBlock: Found 'case' after semicolon, will create new statement")
                      }

                      blockCurrent = afterSemi
                      debug(s"parseBlock: After semicolon, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(whitespaceTok @ Token.Whitespace(_, _)) =>
                      debug("parseBlock: Expression followed by whitespace, checking for newlines and case")

                      // Get next token after whitespace
                      val (_, afterWs) = collectComments(next)

                      // Check if next token is 'case'
                      val hasNextCase = isCaseIdentifier(afterWs.current)

                      // Check if whitespace contains a newline
                      val containsNewline = isNewlineWhitespace(whitespaceTok)

                      // Add statement and advance
                      statements = statements :+ expr
                      blockCurrent = afterWs
                      debug(s"parseBlock: After whitespace, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(t) if isCaseIdentifier(next.current) =>
                      debug("parseBlock: Found 'case' after expression, adding to statements")
                      statements = statements :+ expr
                      blockCurrent = next

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
                if (op == "=>") {
                  Right((ObjectExprClauseOnValue(key, value), afterValue))
                } else { // op == "="
                  // For string literals with "=", convert to identifier
                  key match {
                    case stringLit: ConcreteStringLiteral =>
                      val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                      Right((ObjectExprClause(idKey, value), afterValue))
                    case qualifiedName: QualifiedName =>
                      Right((ObjectExprClause(qualifiedName, value), afterValue))
                    case other =>
                      // This case should never happen due to validation in caller
                      Left(ParseError(s"Expected identifier for object field key with = operator but got: $other", keySourcePos.range.start))
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
