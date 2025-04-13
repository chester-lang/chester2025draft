package chester.readerv2
import chester.i18n.*
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
      case Right(Token.StringLiteral(chars, pos)) => (chars, pos)
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
                       source: SourceOffset,
                       tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    previousNonCommentToken: Option[Token] = None,
    newLineAfterBlockMeansEnds: Boolean = false,
    pendingTokens: Vector[Token.Comment | Token.Whitespace] = Vector.empty
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): LexerState = current match {
    case Right(token: Token.Comment) => 
      copy( index=index + 1, previousToken=Some(token),
        pendingTokens=pendingTokens :+ token)
    case Right(token: Token.Whitespace) =>
      copy( index=index + 1, previousToken=Some(token),
        pendingTokens=pendingTokens :+ token)
    case Right(token) =>
      copy(index=index + 1, previousToken=Some(token), previousNonCommentToken=Some(token))
    case Left(_) =>
      copy(index=index + 1)
  }
  def getAndAdvance: (Either[ParseError, Token], LexerState) = (current, advance())
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

  def collectPending: (Vector[Token.Comment| Token.Whitespace], LexerState) =
    (pendingTokens, clearPendingTokens)

  def clearPendingTokens: LexerState = 
    copy(pendingTokens = Vector.empty)
    
  def skip(): LexerState = current match {
    case Right(_: Token.Comment) => advance().skip()
    case Right(_: Token.Whitespace) => advance().skip()
    case _ => this
  }

  // Helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState =
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)

  override def toString: String =
    t"LexerState(index=$index, current=$current, previousToken=$previousToken, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  private def getStartPos(token: Either[ParseError, Token]): Pos =
    token.fold(_.pos, _.sourcePos.range.start)

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
          t"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
          t.sourcePos.range.start
        )
    )
  }

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

  private val tokenCommentToExpr: Token.Comment => Comment = {
    case Token.Comment(text, sourcePos) =>
      val commentType = if (text.trim.startsWith("//")) {
        CommentType.OneLine
      } else {
        CommentType.MultiLine
      }

      Comment(
        content = text.trim,
        typ = commentType,
        sourcePos = Some(sourcePos)
      )
  }

  var DEBUG = false // Keep DEBUG flag for tests that use it
  private val MAX_LIST_ELEMENTS = 50 // Constants for parser configuration
}

class LexerV2(initState: LexerState, ignoreLocation: Boolean ) {
  import LexerV2._
  var stateVar: LexerState = initState

  private inline def state: LexerState = stateVar
  private inline def execute[T](s: LexerState=>(T, LexerState)): T = {
    val (result, newState) = s(state)
    stateVar = newState
    result
  }
  private inline def d[T](s: LexerState=>LexerState): Unit = stateVar = s(state)
  private inline def pop() = execute(_.getAndAdvance)

  /** Creates expression metadata from source positions and comments. */
  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] =
    if (ignoreLocation) None
    else
      PartialFunction.condOpt((startPos, endPos)) {
        case (Some(start), Some(end)) =>
          ExprMeta(Some(SourcePos(state.source, RangeInFile(start.range.start, end.range.end))), None)
        case (Some(pos), None) =>
          ExprMeta(Some(pos), None)
        case (None, Some(pos)) =>
          ExprMeta(Some(pos), None)
      }

  private def debug(msg: => String): Unit = if (DEBUG) println(t"[DEBUG] $msg")

  // Helper methods
  private def charsToString(chars: Seq[StringChar]): String = chars.map(_.text).mkString

  // Helper for building operator sequences
  def buildOpSeq(terms: Vector[Expr]): Either[ParseError, Expr] = {
    terms match {
      case Vector() => Left(ParseError("Empty operator sequence", getStartPos(state.current)))
      case Vector(expr)  =>
        Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), execute(_.collectPending)))))
      case _            => Right(OpSeq(terms, createMetaWithComments(None, execute(_.collectPending))))
    }
  }

  // Main expression continuation parser
  def parseRest(expr: Expr): Either[ParseError, Expr] = {
    var localTerms = Vector(expr)

    // Handle special closing brace + newline pattern
    if (checkForRBraceNewlinePattern(state)) {
      return buildOpSeq(localTerms)
    }

    d(_.skip())

    if (isAtTerminator(state)) {
      debug("parseRest: Hit terminator token")
      return buildOpSeq(localTerms)
    }

    // Main token dispatch
    state.current match {
      // Match expression handling - treat like any other expression
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == "match" && expr.isInstanceOf[ConcreteIdentifier] =>
        val matchId = ConcreteIdentifier("match", createMeta(None, None))
        d(_.advance().skip())

        // For match blocks, parse using the regular block parser with no special case handling
        withComments(parseBlock)(afterMatch).map { case (block, afterBlock) =>
          // Create the match expression with the block as-is
          val matchExpr = OpSeq(Vector(expr, matchId, block), None)
          (matchExpr, afterBlock)
        }

      // Block argument handling
      case Right(Token.LBrace(braceSourcePos)) =>
        debug("parseRest: Found LBrace after expression, treating as block argument")
        handleBlockArgument(expr, state, localTerms, braceSourcePos)

      // Colon handling (type annotations, etc)
      case Right(Token.Colon(sourcePos)) =>
        debug("parseRest: Found colon")
        handleColon(sourcePos, state, localTerms)

      // Dot call handling
      case Right(Token.Dot(dotSourcePos)) =>
        debug("parseRest: Found dot")
        handleDotCall(dotSourcePos, current, localTerms).flatMap { case (dotCall, newState) =>
          localTerms = Vector(dotCall)
          debug(t"parseRest: After dot call, terms: $localTerms")
          parseRest(dotCall)
        }

      // Operator handling
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseRest: Found operator $op")
        handleOperatorInRest(op, sourcePos, current, localTerms)

      // Identifier handling
      case Right(Token.Identifier(chars, sourcePos)) =>
        val text = charsToString(chars)
        debug(t"parseRest: Found identifier $text")
        handleIdentifierInRest(text, sourcePos, current, localTerms)

      // Generic token handling
      case Right(_) =>
        debug("parseRest: Found other token, parsing as atom")
        withComments(parseAtom)(current).flatMap { case (next, afterNext) =>
          localTerms = localTerms :+ next
          debug(t"parseRest: After parsing other token as atom, terms: $localTerms")
          parseRest(next, afterNext).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(localTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(localTerms, None), finalState)
            }
          }
        }

      // Error handling
      case Left(error) =>
        debug(t"parseRest: Got error: $error")
        Left(error)
    }
  }

  // Handle block arguments
  def handleBlockArgument(expr: Expr, state: LexerState, terms: Vector[Expr], braceSourcePos: SourcePos)(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] =
    withComments(parseBlock)(state).flatMap { case (block, afterBlock) =>
      // Create appropriate expression based on context
      val newExpr = expr match {
        case funcCall: FunctionCall =>
          debug("parseRest: Adding block as argument to existing function call")
          FunctionCall(
            funcCall,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
          )
        case id: ConcreteIdentifier =>
          debug("parseRest: Creating function call with block argument from identifier")
          FunctionCall(
            id,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(afterBlock.sourcePos))
          )
        case _ =>
          debug("parseRest: Default handling for block after expression")
          block
      }

      debug(t"handleBlockArgument: Created expression $newExpr")

      // Handle function calls directly
      if (newExpr.isInstanceOf[FunctionCall]) {
        debug("parseRest: Returning function call with block directly")

        if (afterBlock.isAtTerminator) {
          Right((newExpr, afterBlock))
        } else {
          parseRest(newExpr, afterBlock)
        }
      } else {
        // Handle other expressions via OpSeq
        val updatedTerms = terms.dropRight(1) :+ newExpr
        debug(t"parseRest: After handling block argument, terms: $updatedTerms")

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

  // Colon handling
  def handleColon(sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
    val afterColon = state.advance()
    val updatedTerms = terms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
    debug(t"parseRest: After adding colon, terms: $updatedTerms")

    withComments(parseAtom)(afterColon).flatMap { case (next, afterNext) =>
      val newTerms = updatedTerms :+ next
      debug(t"parseRest: After parsing atom after colon, terms: $newTerms")

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

  // Operator handling
  def handleOperatorInRest(op: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
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
      debug(t"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
      buildOpSeq(updatedTerms)(state, leadingComments).map(result => (result, afterOp))
    } else {
      // Continue parsing the rest of the expression
      withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
        debug(t"parseRest: After parsing atom after operator, got: $next")
        val newTerms = updatedTerms :+ next
        debug(t"parseRest: Updated terms after operator: $newTerms")

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

  // Identifier handling
  def handleIdentifierInRest(text: String, sourcePos: SourcePos, state: LexerState, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
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
          debug(t"parseRest: After function call, terms: $updatedTerms")

          parseRest(functionCall, afterTuple).map { case (result, finalState) =>
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
          debug(t"parseRest: After block in infix, terms: $updatedTerms")

          parseRest(block, afterBlock).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(updatedTerms, None), finalState)
            }
          }
        }
      case Right(Token.Operator(op, opSourcePos)) =>
        debug(t"parseRest: Found operator $op after identifier")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
        val updatedTerms = terms :+ id :+ opId
        debug(t"parseRest: After adding id and op, terms: $updatedTerms")

        val afterOp = afterId.advance()
        withComments(parseAtom)(afterOp).flatMap { case (next, afterNext) =>
          val newTerms = updatedTerms :+ next
          debug(t"parseRest: After parsing atom after operator, terms: $newTerms")

          parseRest(next, afterNext).map { case (result, finalState) =>
            result match {
              case opSeq: OpSeq =>
                (OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None), finalState)
              case _ =>
                (OpSeq(newTerms, None), finalState)
            }
          }
        }
      case _ =>
        debug(t"parseRest: Found bare identifier $text")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val updatedTerms = terms :+ id
        debug(t"parseRest: After adding bare id, terms: $updatedTerms")

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

  // Main parsing methods
  def parseExpr(): Either[ParseError, Expr] = {
    d(_.skip())
    var terms = Vector.empty[Expr]
    debug(t"Starting parseExpr with state: $current")

    // Main parsing logic - handle different token types
    state.current match {
      // Prefix operator
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseExpr: Starting with operator $op")
        d(_.advance())
        state.current match {
          // Function call form: op(args)
          case Right(Token.LParen(_)) =>
            debug("parseExpr: Found lparen after initial operator")
            parseTuple().map { case (tuple, afterTuple) =>
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
              debug(t"parseExpr: After initial operator and atom, terms: $terms")

              if (afterExpr.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right((OpSeq(terms, None), afterExpr))
              } else {
                parseRest(expr, afterExpr)
              }
            }
        }

      // Keyword operator handling
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) =>
        debug(t"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        val afterOp = current.advance()
        terms = Vector(ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))))
        withComments(parseAtom)(afterOp).flatMap { case (expr, afterExpr) =>
          terms = terms :+ expr
          debug(t"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (afterExpr.isAtTerminator) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            Right((OpSeq(terms, None), afterExpr))
          } else {
            parseRest(expr, afterExpr)
          }
        }

      // Standard expression handling
      case _ =>
        debug("parseExpr: Starting with atom")
        withComments(parseAtom)(current).flatMap { case (first, afterFirst) =>
          debug(t"parseExpr: After initial atom, got: $first")
          parseRest(first, afterFirst)
        }
    }
  }

  /** Checks for the pattern of a right brace followed by a newline. This is used to detect block termination in certain contexts.
    */
  private def checkForRBraceNewlinePattern(state: LexerState): Boolean = {
    // First check the preconditions - must be in context where newlines are significant,
    // and previous token must be a closing brace
    val state1 = state.skipComments
    if (!state1.newLineAfterBlockMeansEnds || !state1.previousNonCommentToken.exists(_.isInstanceOf[Token.RBrace])) {
      return false
    }

    // Check if current token contains a newline or is EOF
    val hasNewline = state1.current match {
      case Right(ws: Token.Whitespace) =>
        lazy val wsContent = for {
          source <- state.source.readContent.toOption
          range = ws.sourcePos.range
          if range.start.index.utf16 < source.length && range.end.index.utf16 <= source.length
          content = source.substring(range.start.index.utf16, range.end.index.utf16)
        } yield content

        wsContent.exists(_.contains('\n'))
      case Right(_: Token.EOF)       => true
      case Right(_: Token.Semicolon) => true // Also treat semicolons as terminators
      // For match expressions, always treat a case keyword as a terminator
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == "case" => true
      case _                                                                   => false
    }

    // Log if pattern is detected and debug is enabled
    if (DEBUG && hasNewline) debug("}\n check: pattern detected")
    hasNewline
  }

  private def handleDotCall(dotSourcePos: SourcePos, terms: Vector[Expr]): Either[ParseError, Expr] = {
    val afterDot = state.advance() // Skip the dot
    afterDot.current match {
      case Right(Token.Identifier(chars1, idSourcePos1)) =>
        val afterId = afterDot.advance()
        val field = createIdentifier(chars1, idSourcePos1)
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(state: LexerState): Either[ParseError, Expr] =
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
              Right(createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(state.sourcePos)))
          }

        parseNextTelescope(afterId)
      case Right(Token.Operator(op, idSourcePos)) =>
        val afterOp = afterDot.advance()
        val field = ConcreteIdentifier(op, createMeta(Some(idSourcePos), Some(idSourcePos)))
        afterOp.current match {
          case Right(Token.LParen(_)) =>
            parseTuple(afterOp).map { case (args, afterArgs) =>
              createDotCall(terms.last, field, Vector(args), Some(dotSourcePos), Some(afterArgs.sourcePos))
            }
          case _ =>
            Right(createDotCall(terms.last, field, Vector.empty, Some(dotSourcePos), Some(idSourcePos)))
        }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseAtom(current: LexerState): Either[ParseError, Expr] =
    current.current match {
      case Left(err) => Left(err)
      case LBrace(_) =>
        // Check for empty object or block
        val advance1 = current.advance().skipComments
        advance1.current match {
          case Left(err)                        => Left(err)
          case RBrace(_)                        => parseObject(current) // Empty object
          case Id(_, _) | Sym(_, _) | Str(_, _) =>
            // Look ahead for = or => to determine if it's an object
            val afterId = advance1.advance().skipComments
            afterId.current match {
              case Left(err)                            => Left(err)
              case Op(op, _) if op == "=" || op == "=>" => parseObject(current)
              case _                                    => withComments(parseBlock)(current)
            }
          case _ => withComments(parseBlock)(current)
        }

      case LParen(_) => parseTuple(current)

      case Id(chars, sourcePos) =>
        val afterId = current.advance()
        afterId.current match {
          case LBracket(_) =>
            // Generic type parameters
            val identifier = createIdentifier(chars, sourcePos)
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
            val identifier = createIdentifier(chars, sourcePos)
            parseFunctionCallWithId(identifier, afterId)
          case _ =>
            // Plain identifier
            Right((createIdentifier(chars, sourcePos), afterId))
        }

      case Int(_, _) => parseInt(current)
      case Rat(_, _) => parseRational(current)
      case Str(_, _) => parseString(current)
      case Sym(_, _) => parseSymbol(current)

      case LBracket(_) => withComments(parseList)(current)

      case Right(token) => Left(ParseError(t"Unexpected token: $token", token.sourcePos.range.start))

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

  def parseExprList(): Either[ParseError, Vector[Expr]] = {
    d(_.skip())

    @tailrec
    def parseElements( exprs: Vector[Expr], maxExprs: Int): Either[ParseError, Vector[Expr]] =
      if (exprs.length >= maxExprs) {
        Left(ParseError(t"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", state.sourcePos.range.start))
      } else {
        d(_.skip())
        debug(t"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${current.current}")
        state.current match {
          case Right(token) if isRightDelimiter(token) =>
            debug("Found right delimiter after expression")
            Right(exprs)
          case Right(_: Token.Comma | _: Token.Semicolon) =>
            debug("Found comma or semicolon, skipping")
            d(_.advance().skip())
            parseElements( exprs, maxExprs)
          case _ =>
            debug("Parsing expression")
            parseExpr(current) match {
              case Left(err)                => Left(err)
              case Right(expr) =>
                d(_.skip())

                // Check if we've reached a terminator
                state.current match {
                  case Right(token) if isRightDelimiter(token) =>
                    // Attach trailing comments to the expression if any
                    val updatedExpr =
                      expr.updateMeta { meta =>
                        val newMeta = createMetaWithComments(
                          meta.flatMap(_.sourcePos),
                          Vector.empty,
                          execute(_.collectPending)
                        )
                        // Merge with existing meta
                        mergeMeta(meta, newMeta)
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
                          trailingComments.collect { case c: Comment => c }
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

  private def parseTuple(state: LexerState): Either[ParseError, Tuple] = state.current match {
    case LParen(sourcePos) =>
      d(_.advance().skip())
      for {
        exprs <- parseExprList(afterLParen)
        () = d(_.skip())
        result <- state.current match {
          case RParen(_) =>
            d(_.advance())
            val leading = execute(_.collectPending).collect { case c: Comment => c }
            d(_.skip())
            val trailing = execute(_.collectPending).collect { case c: Comment => c }
            val meta =
                createMeta(Some(sourcePos), Some(state.sourcePos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leading, trailing)))
            Right((Tuple(exprs, meta)))
          case _ => Left(expectedError("right parenthesis", state.current))
        }
      } yield result
    case _ => Left(expectedError("left parenthesis", state.current))
  }

  // Check if a token is a 'case' identifier
  private def isCaseIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(Token.Identifier(chars, _)) => charsToString(chars) == "case"
    case _                                 => false
  }

  private def parseBlock(): Either[ParseError, Block] = {
    d(_.withNewLineTermination(true))
    debug(t"parseBlock: starting with state=$contextState")

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
        debug(t"parseBlock: After opening brace, blockCurrent=$blockCurrent")

        // Regular block parsing - all statements are treated the same
        while (maxExpressions > 0) {
          maxExpressions -= 1

          val (blockComments, withoutComments) = collectComments(blockCurrent)
          blockCurrent = withoutComments

          blockCurrent.current match {
            case Right(Token.RBrace(_)) =>
              debug(t"parseBlock: Found closing brace, statements=${statements.size}, has result=${result.isDefined}")
              val finalBlock = Block(statements, result, None)
              blockCurrent = blockCurrent.advance()
              debug(t"parseBlock: Returning block with ${statements.size} statements, result=${result.isDefined}")
              return Right((finalBlock, blockCurrent))

            case Right(Token.Semicolon(_)) =>
              debug("parseBlock: Found semicolon, advancing")
              val (_, afterSemi) = collectComments(blockCurrent.advance())
              blockCurrent = afterSemi
              debug(t"parseBlock: After semicolon, blockCurrent=$blockCurrent")

            case Right(Token.Whitespace(_, _)) =>
              debug("parseBlock: Found whitespace, advancing")
              val (_, afterWs) = collectComments(blockCurrent.advance())
              blockCurrent = afterWs
              debug(t"parseBlock: After whitespace, blockCurrent=$blockCurrent")

            case _ if isCaseIdentifier(blockCurrent.current) =>
              // When we encounter 'case' at the beginning of an expression, we need to parse it
              // normally but ensure it's a separate statement
              debug("parseBlock: Found 'case' identifier, parsing statement")
              parseExpr(blockCurrent) match {
                case Left(err) =>
                  debug(t"parseBlock: Error parsing case expression: $err")
                  return Left(err)
                case Right((expr, next)) =>
                  debug(t"parseBlock: Parsed case statement: $expr")
                  statements = statements :+ expr

                  // Skip past semicolons
                  val afterExpr = next.current match {
                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Skipping semicolon after case statement")
                      next.advance()
                    case _ => next
                  }

                  blockCurrent = afterExpr
                  debug("parseBlock: Updated block state after case statement")
              }

            case _ =>
              debug(t"parseBlock: Parsing expression at token ${blockCurrent.current}")
              parseExpr(blockCurrent) match {
                case Left(err) =>
                  debug(t"parseBlock: Error parsing expression: $err")
                  return Left(err)
                case Right((expr, next)) =>
                  debug(t"parseBlock: Parsed expression: $expr, next token: ${next.current}")

                  next.current match {
                    case Right(Token.RBrace(_)) =>
                      debug("parseBlock: Expression followed by closing brace, setting as result")
                      // This is the V1 style: put the last expression in the result field
                      result = Some(expr)
                      blockCurrent = next.advance()
                      debug(t"parseBlock: Returning block with ${statements.size} statements and result=$expr")
                      return Right((Block(statements, result, None), blockCurrent))

                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Expression followed by semicolon, adding to statements")
                      statements = statements :+ expr

                      // Check for case after semicolon
                      val (_, afterSemi) = collectComments(next.advance())

                      blockCurrent = afterSemi
                      debug(t"parseBlock: After semicolon, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(whitespaceTok @ Token.Whitespace(_, _)) =>
                      debug("parseBlock: Expression followed by whitespace, checking for newlines and case")

                      // Get next token after whitespace
                      val (_, afterWs) = collectComments(next)

                      // Add statement and advance
                      statements = statements :+ expr
                      blockCurrent = afterWs
                      debug(t"parseBlock: After whitespace, statements=${statements.size}, blockCurrent=$blockCurrent")

                    case Right(t) if isCaseIdentifier(next.current) =>
                      debug("parseBlock: Found 'case' after expression, adding to statements")
                      statements = statements :+ expr
                      blockCurrent = next

                    case Right(t) =>
                      debug(t"parseBlock: Unexpected token after expression: $t")
                      return Left(ParseError("Expected ';', whitespace, or '}' after expression in block", t.sourcePos.range.start))

                    case Left(err) =>
                      debug(t"parseBlock: Error after expression: $err")
                      return Left(err)
                  }
              }
          }
        }

        debug("parseBlock: Too many expressions in block")
        Left(ParseError("Too many expressions in block", current.sourcePos.range.start))
      case Right(t) =>
        debug(t"parseBlock: Expected '{' but found $t")
        Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) =>
        debug(t"parseBlock: Error at start of block: $err")
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

  private def parseObject(): Either[ParseError, ObjectExpr] = {
    state.current match {
      case Right(Token.LBrace(sourcePos)) =>
        // Collect comments after the opening brace
        val (afterBraceComments, afterBrace) = collectComments(current.advance())

        def parseFields(state: LexerState, clauses: Vector[ObjectClause]): Either[ParseError, (Vector[ObjectClause], LexerState)] =
          state.current match {
            case Right(Token.RBrace(_)) =>
              Right((clauses, state))
            case Right(Token.Identifier(chars, idSourcePos)) =>
              val identifier = ConcreteIdentifier(charsToString(chars), createMeta(Some(idSourcePos), Some(idSourcePos)))
              parseField(state.advance().skipComments, identifier, idSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap(nextStateAfterComma => parseFields(nextStateAfterComma, clauses :+ clause))
              }
            case Right(Token.StringLiteral(chars, strSourcePos)) =>
              val stringLiteral = ConcreteStringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
              parseField(state.advance().skipComments, stringLiteral, strSourcePos).flatMap { case (clause, nextState) =>
                checkAfterField(nextState).flatMap(nextStateAfterComma => parseFields(nextStateAfterComma, clauses :+ clause))
              }
            case Right(Token.SymbolLiteral(value, symSourcePos)) =>
              val symbolLiteral = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
              parseField(state.advance().skipComments, symbolLiteral, symSourcePos).flatMap { case (clause, nextState) =>
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
                      Left(ParseError(t"Expected identifier for object field key with = operator but got: $other", keySourcePos.range.start))
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
            Left(ParseError(t"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", sourcePos.range.start))
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
  
  type CommOrWhite = Comment | Token.Whitespace

  extension (s: LexerState) {
    @deprecated("please handle it correctly")
    def skipComments: LexerState = collectComments(s)._2
  }
  
  /** Collects comments from the current state. Returns a tuple of (collected comments, updated state).
    */
  private def collectComments(state: LexerState): (Vector[CommOrWhite], LexerState) = {
    // First check if we have any pending tokens from the state
    val pendingTokens = state.pendingTokens
    val startState = if (pendingTokens.nonEmpty) state.clearPendingTokens() else state
    
    @tailrec
    def collectRec(current: LexerState, comments: Vector[CommOrWhite]): (Vector[CommOrWhite], LexerState) =
      if (!current.isAtEnd && current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
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
          case Right(wt@Token.Whitespace(_, _)) =>
            collectRec(current.advance(), comments :+ wt)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        (comments, current)
      }

    // Combine any pending tokens with collected tokens
    val (collectedComments, finalState) = collectRec(startState, Vector.empty)
    val allComments = pendingTokens.map {
      case c: Token.Comment => 
        val commentType = if (c.text.trim.startsWith("//")) {
          CommentType.OneLine
        } else {
          CommentType.MultiLine
        }
        
        Comment(
          content = c.text.trim,
          typ = commentType,
          sourcePos = Some(c.sourcePos)
        ): CommOrWhite
      case w: Token.Whitespace => w: CommOrWhite
    } ++ collectedComments
    
    (allComments, finalState)
  }

  /** Creates ExprMeta with comments.
    */
  private def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[CommOrWhite] = Vector.empty,
      trailingComments: Vector[CommOrWhite] = Vector.empty
  ): Option[ExprMeta] =
     {
      ExprMeta.maybe(sourcePos, createCommentInfo(leadingComments, trailingComments))
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
      parseMethod: => Either[ParseError, T]
  ): Either[ParseError, T] = {
    d(_.skip())

    // Parse the expression using the provided method
    parseMethod.map { expr =>
      val leadingComments = execute(_.collectPending)
      d(_.skip())
      val trailingComments = execute(_.collectPending)

        expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )

          // Merge the existing meta with new comment information
          mergeMeta(existingMeta, newMeta)
        }
    }
  }

  private def createCommentInfo(
      leadingComments: Vector[CommOrWhite],
      trailingComments: Vector[CommOrWhite] = Vector.empty
  ): Option[CommentInfo] =
    {
      CommentInfo.maybe(
        commentBefore = leadingComments.collect { case c: Comment => c },
        commentInBegin = Vector.empty,
        commentInEnd = Vector.empty,
        commentEndInThisLine = trailingComments.collect { case c: Comment => c }
      )
    }

  // Helper for handling common error patterns
  private def withErrorHandling[T](
      errorMsg: String
  )( parser: => Either[ParseError, T]): Either[ParseError, T] =
    parser match {
      case Left(err) => Left(ParseError(t"$errorMsg: ${err.message}", err.pos))
      case right     => right
    }

  // Helper that combines collecting comments and parsing with error handling
  private def withCommentsAndErrorHandling[T <: Expr](
      errorMsg: String
  )(parser:  => Either[ParseError, T]): Either[ParseError, T] = withErrorHandling(errorMsg){withComments{parser}}

  private def parseString(): Either[ParseError, Expr] =
    withCommentsAndErrorHandling("Error parsing string"){
      parseLiteral(
        token =>
          PartialFunction.condOpt(token) { case Token.StringLiteral(chars, sourcePos) =>
            (charsToString(chars), sourcePos)
          },
        (value, meta) => ConcreteStringLiteral(value, meta),
        "Expected string literal"
      )
    }

  // Create a helper method for parsing literals with common pattern
  private def parseLiteral[T <: Expr](
      extract: Token => Option[(String, SourcePos)],
      create: (String, Option[ExprMeta]) => T,
      errorMsg: String
  ): Either[ParseError, T] =
    state.current match {
      case Right(token) =>
        extract(token) match {
          case Some((value, sourcePos)) =>
            val meta = createMeta(Some(sourcePos), Some(sourcePos))
            d(_.advance())
            Right(create(value, meta))
          case None =>
            Left(ParseError(errorMsg, state.sourcePos.range.start))
        }
      case Left(err) => Left(err)
    }

  // Helper functions for other literal types
  def parseInt(state: LexerState): Either[ParseError, Expr] =
    withCommentsAndErrorHandling("Error parsing integer") {
      parseLiteral(
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
      )
    }

  private def parseSymbol(state: LexerState): Either[ParseError, Expr] =
    withCommentsAndErrorHandling("Error parsing symbol"){
      parseLiteral(
        st,
        token =>
          PartialFunction.condOpt(token) { case Token.SymbolLiteral(value, sourcePos) =>
            (value, sourcePos)
          },
        chester.syntax.concrete.SymbolLiteral.apply,
        "Expected symbol literal"
      )
    }

  // Helper to create identifier expressions
  private def createIdentifier(chars: Vector[StringChar], sourcePos: SourcePos): ConcreteIdentifier =
    ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos)))

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
