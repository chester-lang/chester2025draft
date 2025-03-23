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
import chester.readerv2.Token.*

import scala.reflect.ClassTag

case class StmtExpr(expr: Expr)

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
  def isAfterClosingBrace: Boolean = previousToken.exists(_.isInstanceOf[Token.RBrace])
  def isAtNewline: Boolean = current.exists {
    case Token.Whitespace(_, hasNewline) => hasNewline
    case Token.EOF(_)                    => true
    case _                               => false
  }
  def isAtClosingBraceNewlinePattern: Boolean = isAfterClosingBrace && isAtNewline
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
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerV2 =
    new LexerV2(tokens, sourceOffset, ignoreLocation)

  var DEBUG = false // Keep DEBUG flag for tests that use it
  val MAX_LIST_ELEMENTS = 50 // Constants for parser configuration
}

class LexerV2(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean) {
  import LexerV2.DEBUG

  private def debug(msg: => String): Unit = if (DEBUG) println(s"[DEBUG] $msg")

  // Helper methods
  private def charsToString(chars: Seq[StringChar]): String = chars.map(_.text).mkString

  private def createParseError(msg: String, token: Either[ParseError, Token]): ParseError = 
    token.fold(
      identity, 
      t => ParseError(s"$msg at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}", t.sourcePos.range.start)
    )

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
      t => ParseError(
        s"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
        t.sourcePos.range.start
      )
    )
  }

  /** Creates expression metadata from source positions and comments.
    */
  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] = {
    if (ignoreLocation) None
    else (startPos, endPos) match {
      case (Some(start), Some(end)) =>
        Some(ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(start.range.start, end.range.end))), None))
      case (Some(pos), None) =>
        Some(ExprMeta(Some(pos), None))
      case (None, Some(pos)) =>
        Some(ExprMeta(Some(pos), None))
      case _ =>
        None
    }
  }

  private def getSourcePos(token: Either[ParseError, Token]): SourcePos = {
    debug(s"getSourcePos: token=$token")
    token.fold(
      err => err.sourcePos.getOrElse(SourcePos(SourceOffset(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero))),
      t => t.sourcePos
    )
  }

  private def getStartPos(token: Either[ParseError, Token]): Pos = 
    token.fold(_.pos, _.sourcePos.range.start)

  // Type alias for clarity
  type LexerError = ParseError

  // Main parsing methods
  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    val (leadingComments, current) = collectComments(state)
    var terms = Vector.empty[Expr]
    debug(s"Starting parseExpr with state: $current")

    def buildOpSeq(terms: Vector[Expr]): Either[ParseError, Expr] = {
      debug(s"Building OpSeq with terms: $terms")
      if (terms.isEmpty) {
        Left(ParseError("Empty operator sequence", getStartPos(state.current)))
      } else if (terms.length == 1) {
        val expr = terms.head
        // Attach leading comments to single expression if present
        if (leadingComments.nonEmpty) {
          Right(expr.updateMeta { meta =>
            val newMeta = createMetaWithComments(meta.flatMap(_.sourcePos), leadingComments)
            mergeMetaWithComments(meta, newMeta)
          })
        } else {
          Right(expr)
        }
      } else {
        // Create OpSeq with the leading comments
        val opSeqMeta = Option.when(leadingComments.nonEmpty)(createMetaWithComments(None, leadingComments).get)
        Right(OpSeq(terms, opSeqMeta))
      }
    }

    def mergeMetaWithComments(existing: Option[ExprMeta], newMeta: Option[ExprMeta]): Option[ExprMeta] = {
      (existing, newMeta) match {
        case (Some(existing), Some(ExprMeta(newSourcePos, newCommentInfo))) =>
          val mergedSourcePos = existing.sourcePos.orElse(newSourcePos)
          val mergedCommentInfo = (existing.commentInfo, newCommentInfo) match {
            case (Some(existingInfo), Some(newInfo)) =>
              Some(
                chester.syntax.concrete.CommentInfo(
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
    }

    def parseRest(expr: Expr, state: LexerState): Either[ParseError, (Expr, LexerState)] = {
      var localTerms = Vector(expr)
      debug(s"parseRest called with expr: $expr, state: $state, current terms: $localTerms")

      // Check for "}\n" pattern
      def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
        // Only consider }\n as terminating if we're in the right context
        if (!state.newLineAfterBlockMeansEnds) return false

        expr match {
          case block: Block =>
            state.current match {
              case Right(whitespaceToken: Token.Whitespace) => {
                val startPos = whitespaceToken.sourcePos.range.start.index.utf16
                val endPos = whitespaceToken.sourcePos.range.end.index.utf16
                val maybeSource = sourceOffset.readContent.toOption
                maybeSource.exists { source =>
                  if (startPos < source.length && endPos <= source.length) {
                    val whitespaceText = source.substring(startPos, endPos)
                    whitespaceText.contains('\n')
                  } else false
                }
              }
              case _ => false
            }
          case _ => false
        }
      }

      if (checkForRBraceNewlinePattern(state)) {
        debug("parseRest: Terminating expression due to }\n pattern")
        return buildOpSeq(localTerms).map(result => (result, state))
      }

      val (restComments, current) = collectComments(state)

      // Handle terminators
      if (isAtTerminator(current)) {
        debug("parseRest: Hit terminator token")
        return buildOpSeq(localTerms).map(result => (result, current))
      }

      current.current match {
        // Handle block after expression
        case Right(Token.LBrace(braceSourcePos)) => {
          debug("parseRest: Found LBrace after expression, treating as block argument")
          handleBlockArgument(expr, state, localTerms, braceSourcePos)
        }

        // Handle colon
        case Right(Token.Colon(sourcePos)) => {
          debug("parseRest: Found colon")
          handleColon(sourcePos, state, localTerms)
        }

        // Handle dot call
        case Right(Token.Dot(dotSourcePos)) => {
          debug("parseRest: Found dot")
          handleDotCall(dotSourcePos, current, localTerms).flatMap { case (dotCall, newState) =>
            localTerms = Vector(dotCall)
            debug(s"parseRest: After dot call, terms: $localTerms")
            parseRest(dotCall, newState)
          }
        }

        // Handle operators
        case Right(Token.Operator(op, sourcePos)) => {
          debug(s"parseRest: Found operator $op")
          handleOperatorInRest(op, sourcePos, current, localTerms)
        }

        // Handle identifiers
        case Right(Token.Identifier(chars, sourcePos)) => {
          val text = charsToString(chars)
          debug(s"parseRest: Found identifier $text")
          handleIdentifierInRest(text, sourcePos, current, localTerms)
        }

        // Handle other tokens
        case Right(_) => {
          debug("parseRest: Found other token, parsing as atom")
          parseAtomWithComments(current).flatMap { case (next, afterNext) =>
            localTerms = localTerms :+ next
            debug(s"parseRest: After parsing other token as atom, terms: $localTerms")
            parseRest(next, afterNext).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(localTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(localTerms, None), finalState)
              }
            }
          }
        }

        // Handle errors
        case Left(error) => {
          debug(s"parseRest: Got error: $error")
          Left(error)
        }
      }
    }

    def handleBlockArgument(expr: Expr, state: LexerState, terms: Vector[Expr], braceSourcePos: SourcePos): Either[ParseError, (Expr, LexerState)] = {
      parseBlockWithComments(state).flatMap { case (block, afterBlock) =>
        // Handle block argument differently based on previous expression type
        val newExpr = expr match {
          // If previous expression is already a function call, add the block as another argument
          case funcCall: FunctionCall => {
            debug("parseRest: Adding block as argument to existing function call")
            FunctionCall(
              funcCall,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          }
          // If previous expression is an identifier, create a function call with block argument
          case id: ConcreteIdentifier => {
            debug("parseRest: Creating function call with block argument from identifier")
            FunctionCall(
              id,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
            )
          }
          // Otherwise, just add the block as a term
          case _ => {
            debug("parseRest: Default handling for block after expression")
            block
          }
        }

        // For function calls with blocks, don't wrap in OpSeq
        if (
          (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[ConcreteIdentifier]) ||
          (newExpr.isInstanceOf[FunctionCall] && expr.isInstanceOf[FunctionCall])
        ) {
          debug("parseRest: Returning function call with block directly")

          if (afterBlock.isAtTerminator) {
            // No more tokens, return the function call directly
            Right((newExpr, afterBlock))
          } else {
            // More tokens follow, continue parsing
            parseRest(newExpr, afterBlock)
          }
        } else {
          // Replace the last term with the new expression that includes the block
          val updatedTerms = terms.dropRight(1) :+ newExpr
          debug(s"parseRest: After handling block argument, terms: $updatedTerms")

          parseRest(newExpr, afterBlock).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      }
    }

    def handleColon(sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterColon = state.advance()
      val updatedTerms = terms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
      debug(s"parseRest: After adding colon, terms: $updatedTerms")

      parseAtomWithComments(afterColon).flatMap { case (next, afterNext) =>
        val newTerms = updatedTerms :+ next
        debug(s"parseRest: After parsing atom after colon, terms: $newTerms")

        parseRest(next, afterNext).map { case (result, finalState) =>
          result match {
            case opSeq: OpSeq =>
              (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
            case _ =>
              (OpSeq(newTerms, None), finalState)
          }
        }
      }
    }

    def handleOperatorInRest(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterOp = state.advance()

      // Check if this is a terminating operator (like * in vararg context)
      if (op == "*" && isVarargContext(state)) {
        val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))
        debug(s"parseRest: Added terminating operator *, terms: $updatedTerms")
        buildOpSeq(updatedTerms).map(result => (result, afterOp))
      } else {
        // Add operator to terms
        val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))

        parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
          debug(s"parseRest: After parsing atom after operator, got: $next")
          val newTerms = updatedTerms :+ next
          debug(s"parseRest: Updated terms after operator: $newTerms")

          // Continue parsing the rest
          parseRest(next, afterNext).map { case (result, finalState) =>
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

    def handleIdentifierInRest(text: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr]): Either[ParseError, (Expr, LexerState)] = {
      val afterId = state.advance()

      afterId.current match {
        case Right(Token.LParen(_)) => {
          debug("parseRest: Found lparen after identifier")
          parseTuple(afterId).flatMap { case (tuple, afterTuple) =>
            val functionCall = FunctionCall(
              ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
              tuple,
              createMeta(Some(sourcePos), Some(sourcePos))
            )
            val updatedTerms = terms :+ functionCall
            debug(s"parseRest: After function call, terms: $updatedTerms")

            parseRest(functionCall, afterTuple).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }
        }
        case Right(Token.LBrace(_)) => {
          debug("parseRest: Found lbrace after identifier")
          parseBlockWithComments(afterId).flatMap { case (block, afterBlock) =>
            // In V1 parser, a block after an identifier in infix is treated as part of the OpSeq
            val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
            val updatedTerms = terms :+ id :+ block
            debug(s"parseRest: After block in infix, terms: $updatedTerms")

            parseRest(block, afterBlock).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(updatedTerms, None), finalState)
              }
            }
          }
        }
        case Right(Token.Operator(op, opSourcePos)) => {
          debug(s"parseRest: Found operator $op after identifier")
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
          val updatedTerms = terms :+ id :+ opId
          debug(s"parseRest: After adding id and op, terms: $updatedTerms")

          val afterOp = afterId.advance()
          parseAtomWithComments(afterOp).flatMap { case (next, afterNext) =>
            val newTerms = updatedTerms :+ next
            debug(s"parseRest: After parsing atom after operator, terms: $newTerms")

            parseRest(next, afterNext).map { case (result, finalState) =>
              result match {
                case opSeq: OpSeq =>
                  (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
                case _ =>
                  (OpSeq(newTerms, None), finalState)
              }
            }
          }
        }
        case _ => {
          debug(s"parseRest: Found bare identifier $text")
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val updatedTerms = terms :+ id
          debug(s"parseRest: After adding bare id, terms: $updatedTerms")

          parseRest(id, afterId).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      }
    }

    // Main parseExpr logic
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

              if (afterExpr.isAtTerminator) {
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

          if (afterExpr.isAtTerminator) {
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
            parseListWithComments(afterId).flatMap { case (typeParams, afterTypeParams) =>
              // Now check if there are parentheses for function arguments
              afterTypeParams.current match {
                case Right(Token.LParen(_)) => {
                  // Function call with generic type parameters and arguments
                  parseTuple(afterTypeParams).map { case (tuple, afterArgs) =>
                    // Create nested function call: func[T](args) -> FunctionCall(FunctionCall(func, [T]), (args))
                    // Use the original typeParams expression, but convert to ListExpr if needed
                    val typeParamsList = typeParams match {
                      case list: ListExpr => list
                      case other => throw new RuntimeException(s"Expected ListExpr but got ${other.getClass.getSimpleName}")
                    }
                    val funcWithGenericTypes = FunctionCall(
                      identifier,
                      typeParamsList,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )
                    (
                      FunctionCall(
                        funcWithGenericTypes,
                        tuple,
                        createMeta(Some(sourcePos), Some(afterArgs.sourcePos))
                      ),
                      afterArgs
                    )
                  }
                }
                case _ => {
                  // Just the generic type parameters without function arguments
                  // Use the original typeParams expression, but convert to ListExpr if needed
                  val typeParamsList = typeParams match {
                    case list: ListExpr => list
                    case other => throw new RuntimeException(s"Expected ListExpr but got ${other.getClass.getSimpleName}")
                  }
                  Right(
                    (
                      FunctionCall(
                        identifier,
                        typeParamsList,
                        createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                      ),
                      afterTypeParams
                    )
                  )
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
      case Left(error)  => Left(error)
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
    val (leadingListComments, initialState) = collectComments(state)
    
    def parseElements(current: LexerState, exprs: Vector[Expr], maxExprs: Int): Either[ParseError, (Vector[Expr], LexerState)] = {
      if (exprs.length >= maxExprs) {
        Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", state.sourcePos.range.start))
      } else {
        debug(s"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
        current.current match {
          case Right(token) if isRightDelimiter(token) => {
            debug("Found right delimiter after expression")
            Right((exprs, current))
          }
          case Right(_: Token.Comment | _: Token.Whitespace) => {
            // Collect comments instead of skipping them
            val (comments, afterComments) = collectComments(current)
            parseElements(afterComments, exprs, maxExprs)
          }
          case Right(_: Token.Comma | _: Token.Semicolon) => {
            debug("Found comma or semicolon, skipping")
            // Collect any comments after comma/semicolon
            val (_, afterDelimiter) = collectComments(current.advance())
            parseElements(afterDelimiter, exprs, maxExprs)
          }
          case _ => {
            debug("Parsing expression")
            parseExpr(current).flatMap { case (expr, afterExpr) =>
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
                          Some(
                            chester.syntax.concrete.CommentInfo(
                              commentBefore = existingInfo.commentBefore,
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
                }
              } else {
                expr
              }

              afterComments.current match {
                case Right(token) if isRightDelimiter(token) => {
                  debug("Found right delimiter after expression")
                  Right((exprs :+ updatedExpr, afterComments))
                }
                case Right(_: Token.Comma | _: Token.Semicolon) => {
                  debug("Found comma or semicolon after expression, advancing")
                  // Collect any comments after comma/semicolon
                  val (_, afterDelimiter) = collectComments(afterComments.advance())
                  parseElements(afterDelimiter, exprs :+ updatedExpr, maxExprs - 1)
                }
                case Right(_: Token.Comment | _: Token.Whitespace) => {
                  // Collect comments instead of skipping them
                  val (comments, afterMoreComments) = collectComments(afterComments)
                  parseElements(afterMoreComments, exprs :+ updatedExpr, maxExprs)
                }
                case Right(t) => Left(expectedError("',' or ')' after expression", Right(t)))
                case Left(err) => Left(err)
              }
            }
          }
        }
      }
    }
    
    parseElements(initialState, Vector.empty, LexerV2.MAX_LIST_ELEMENTS)
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
                  val allLeadingComments = leadingComments ++ trailingComments
                  val commentInfo = PartialFunction.condOpt((leadingComments.nonEmpty, trailingComments.nonEmpty)) {
                    case (true, true) =>
                      chester.syntax.concrete.CommentInfo(
                        commentBefore = allLeadingComments,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = trailingComments
                      )
                    case (true, false) =>
                      chester.syntax.concrete.CommentInfo(
                        commentBefore = allLeadingComments,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = Vector.empty
                      )
                    case (false, true) =>
                      chester.syntax.concrete.CommentInfo(
                        commentBefore = Vector.empty,
                        commentInBegin = Vector.empty,
                        commentInEnd = Vector.empty,
                        commentEndInThisLine = trailingComments
                      )
                  }

                  ExprMeta(m.sourcePos, commentInfo)
                }
              } else {
                createMeta(Some(sourcePos), Some(afterList.sourcePos))
              }

              Right((Tuple(tupleExprs, tupleMeta), afterList.advance()))
            }
            case Right(t)  => Left(ParseError("Expected right parenthesis", t.sourcePos.range.start))
            case Left(err) => Left(err)
          }
        }
      }
      case Right(t)  => Left(ParseError("Expected left parenthesis", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  def parseBlock(state: LexerState): Either[ParseError, (Block, LexerState)] = {
    // IMPORTANT: Enable newLineAfterBlockMeansEnds for all blocks
    // This preserves uniform treatment without special-casing any operators
    val contextState = state.withNewLineTermination(true)

    // Replace skipComments with collectComments
    val (_, current) = collectComments(contextState)
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
            case Right(Token.Whitespace(_, _)) => {
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
                      // This is the V1 style: put the last expression in the result field
                      // to match the V1 parser's behavior for blocks
                      result = Some(expr)
                      blockCurrent = next.advance()
                      return Right((Block(statements, result, None), blockCurrent))
                    }
                    case Right(Token.Semicolon(_)) | Right(Token.Whitespace(_, _)) => {
                      // Add the expression to statements for all but the last one
                      statements = statements :+ expr
                      // Collect comments after statement
                      val (_, afterStmt) = collectComments(next)
                      blockCurrent = afterStmt
                    }
                    case Right(t)  => return Left(ParseError("Expected ';', whitespace, or '}' after expression in block", t.sourcePos.range.start))
                    case Left(err) => return Left(err)
                  }
                }
              }
            }
          }
        }
        Left(ParseError("Too many expressions in block", current.sourcePos.range.start))
      }
      case Right(t)  => return Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
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
            case Right(Token.Comma(_)) => {
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
          parseExpr(state).flatMap { case (value, afterValue) =>
            state = afterValue
            if (op == "=>") {
              clauses = clauses :+ ObjectExprClauseOnValue(key, value)
              checkAfterField()
            } else { // op == "="
              // For string literals with "=", convert to identifier
              key match {
                case stringLit: ConcreteStringLiteral =>
                  val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                  clauses = clauses :+ ObjectExprClause(idKey, value)
                  checkAfterField()
                case qualifiedName: QualifiedName =>
                  clauses = clauses :+ ObjectExprClause(qualifiedName, value)
                  checkAfterField()
                case other =>
                  // This shouldn't happen with proper validation upstream
                  Left(ParseError(s"Expected identifier for object field key with = operator but got: $other", keySourcePos.range.start))
              }
            }
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
                val commentInfo = Option.when(allLeadingComments.nonEmpty)(
                  chester.syntax.concrete.CommentInfo(
                    commentBefore = allLeadingComments,
                    commentInBegin = Vector.empty,
                    commentInEnd = Vector.empty,
                    commentEndInThisLine = Vector.empty
                  )
                )

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

    initialState.current match {
      case Right(Token.LBracket(sourcePos)) => {
        val (afterBracketComments, afterBracket) = collectComments(initialState.advance())
        
        def parseElements(current: LexerState, exprs: Vector[Expr]): Either[ParseError, (Vector[Expr], LexerState)] = {
          if (exprs.length >= LexerV2.MAX_LIST_ELEMENTS) {
            Left(ParseError(s"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", sourcePos.range.start))
          } else {
            current.current match {
              case Right(Token.RBracket(_)) => 
                Right((exprs, current))
              case Right(Token.Comma(_)) => 
                // Collect comments after comma
                val (_, afterComma) = collectComments(current.advance())
                parseElements(afterComma, exprs)
              case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_, _)) => 
                // Collect comments
                val (_, afterComments) = collectComments(current)
                parseElements(afterComments, exprs)
              case _ => 
                parseExpr(current).flatMap { case (expr, afterExpr) =>
                  // Collect any comments after the expression
                  val (_, afterComments) = collectComments(afterExpr)
                  
                  afterComments.current match {
                    case Right(Token.RBracket(_)) => 
                      Right((exprs :+ expr, afterComments))
                    case Right(Token.Comma(_)) => 
                      val (_, afterComma) = collectComments(afterComments.advance())
                      parseElements(afterComma, exprs :+ expr)
                    case Right(t) => 
                      Left(expectedError("',' or ']' in list", Right(t)))
                    case Left(err) => 
                      Left(err)
                  }
                }
            }
          }
        }
        
        parseElements(afterBracket, Vector.empty).flatMap { case (exprs, finalState) =>
          finalState.current match {
            case Right(Token.RBracket(endPos)) => {
              // Add comments to list meta
              val listMeta = if (leadingComments.nonEmpty || afterBracketComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map { m =>
                  val allLeadingComments = leadingComments ++ afterBracketComments
                  val commentInfo = Option.when(allLeadingComments.nonEmpty)(
                    chester.syntax.concrete.CommentInfo(
                      commentBefore = allLeadingComments
                    )
                  )

                  ExprMeta(m.sourcePos, commentInfo)
                }
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }

              Right((ListExpr(exprs, listMeta), finalState.advance()))
            }
            case Right(t) => 
              Left(ParseError("Expected ']' at end of list", t.sourcePos.range.start))
            case Left(err) => 
              Left(err)
          }
        }
      }
      case Right(t) => 
        Left(ParseError("Expected '[' at start of list", t.sourcePos.range.start))
      case Left(err) => 
        Left(err)
    }
  }

  def collectIdentifier(state: LexerState): (Vector[StringChar], LexerState) = {
    @scala.annotation.tailrec
    def collectRec(current: LexerState, chars: Vector[StringChar]): (Vector[StringChar], LexerState) = {
      if (!current.isAtEnd && current.current.exists(token => token.isInstanceOf[Token.Identifier])) {
        current.current match {
          case Right(id: Token.Identifier) =>
            collectRec(current.advance(), chars ++ id.parts)
          case _ => 
            throw new RuntimeException("Unreachable: exists check guarantees we have an Identifier token")
        }
      } else {
        (chars, current)
      }
    }
    
    collectRec(state, Vector.empty)
  }

  def isIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(token) => { token.isInstanceOf[Token.Identifier] }
    case _            => { false }
  }

  def expectIdentifier(expected: String, state: LexerState): Either[ParseError, LexerState] = {
    state.current match {
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == expected => {
        Right(state.advance())
      }
      case other => {
        Left(ParseError(s"Expected identifier '$expected' but got $other", state.sourcePos.range.start))
      }
    }
  }

  private def expectToken[T <: Token](state: LexerState)(using tag: ClassTag[T]): Either[ParseError, LexerState] = {
    state.current.fold(
      err => Left(err),
      token => {
        val expectedClassName = tag.runtimeClass.getSimpleName
        val actualClassName = token.getClass.getSimpleName
        
        if (tag.runtimeClass.isInstance(token)) {
          Right(state.advance())
        } else {
          Left(ParseError(s"Expected token of type $expectedClassName but got: $actualClassName", token.sourcePos.range.start))
        }
      }
    )
  }

  def isVarargContext(state: LexerState): Boolean = {
    // Check if we're in a function call argument list or type annotation
    val lookAhead = state.advance()
    lookAhead.current match {
      case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
      case _                                              => false
    }
  }

  def skipComments(state: LexerState): LexerState = {
    @scala.annotation.tailrec
    def skipRec(current: LexerState): LexerState = {
      if (current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
        skipRec(current.advance())
      } else {
        current
      }
    }
    
    skipRec(state)
  }

  /** Collects comments from the current state. Returns a tuple of (collected comments, updated state).
    */
  def collectComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    @scala.annotation.tailrec
    def collectRec(current: LexerState, comments: Vector[chester.syntax.concrete.Comment]): (Vector[chester.syntax.concrete.Comment], LexerState) = {
      if (!current.isAtEnd && current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
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
            collectRec(current.advance(), comments :+ comment)
          case Right(Token.Whitespace(_, _)) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just advance the token - we'll hit another token eventually
            collectRec(current.advance(), comments)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }
    }
    
    collectRec(state, Vector.empty)
  }

  /** Collects trailing comments after an expression until a newline or non-comment token.
    */
  def collectTrailingComments(state: LexerState): (Vector[chester.syntax.concrete.Comment], LexerState) = {
    // For trailing comments, we only collect comments that appear on the same line
    // (until we hit a newline in whitespace)
    @scala.annotation.tailrec
    def collectRec(current: LexerState, comments: Vector[chester.syntax.concrete.Comment], hitNewline: Boolean): (Vector[chester.syntax.concrete.Comment], LexerState) = {
      if (!current.isAtEnd && !hitNewline &&
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
            collectRec(current.advance(), comments :+ comment, hitNewline)
          case Right(Token.Whitespace(_, _)) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just assume any whitespace might contain a newline and stop collecting
            collectRec(current.advance(), comments, true)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }
    }
    
    collectRec(state, Vector.empty, false)
  }

  /** Creates ExprMeta with comments.
    */
  def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[chester.syntax.concrete.Comment],
      trailingComments: Vector[chester.syntax.concrete.Comment] = Vector.empty
  ): Option[ExprMeta] = {
    if (sourcePos.isEmpty && leadingComments.isEmpty && trailingComments.isEmpty) {
      None
    } else {
      val commentInfo = Option.unless(leadingComments.isEmpty && trailingComments.isEmpty)(
        chester.syntax.concrete.CommentInfo(
          commentBefore = leadingComments,
          commentEndInThisLine = trailingComments
        )
      )
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
            Right(
              (
                FunctionCall(
                  identifier,
                  Tuple(listExpr.toVector, createMeta(Some(sourcePos), Some(endPos))),
                  createMeta(Some(sourcePos), Some(endPos))
                ),
                afterList.advance()
              )
            )
          }
          case Right(t)  => Left(ParseError("Expected right parenthesis", t.sourcePos.range.start))
          case Left(err) => Left(err)
        }
      }
    }
  }

  /** Parse an atom expression with comments. This collects leading and trailing comments and attaches them to the expression.
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
                  Some(
                    chester.syntax.concrete.CommentInfo(
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
        }
      } else {
        expr
      }

      Right((updatedExpr, finalState))
    }
  }

  /** Parse a block with comments.
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
                  Some(
                    chester.syntax.concrete.CommentInfo(
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
        }
      } else {
        block
      }

      Right((updatedBlock, finalState))
    }
  }

  /** Parse a list with comments.
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
                  Some(
                    chester.syntax.concrete.CommentInfo(
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
        }
      } else {
        list
      }

      Right((updatedList, finalState))
    }
  }
}
