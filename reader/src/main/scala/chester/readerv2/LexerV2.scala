package chester.readerv2
import chester.i18n.*
import chester.error.{Pos, RangeInFile, SourcePos}
import chester.reader.{ParseError, Source}
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
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
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
      LexerState(tokens, index + 1, Some(token), previousNonCommentToken, newLineAfterBlockMeansEnds, pendingTokens :+ token)
    case Right(token: Token.Whitespace) =>
      LexerState(tokens, index + 1, Some(token), previousNonCommentToken, newLineAfterBlockMeansEnds, pendingTokens :+ token)
    case Right(token) =>
      LexerState(tokens, index + 1, Some(token), Some(token), newLineAfterBlockMeansEnds, pendingTokens)
    case Left(_) =>
      LexerState(tokens, index + 1, previousToken, previousNonCommentToken, newLineAfterBlockMeansEnds, pendingTokens)
  }
  def sourcePos: SourcePos = current match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(Source(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t)  => t.sourcePos
  }

  // Helper methods for common state checks
  def isAtTerminator: Boolean = current.exists(t =>
    t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] ||
      t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] ||
      t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon]
  )

  // Methods for handling pending tokens
  def collectPendingComments(): Vector[Token.Comment] =
    pendingTokens.collect { case c: Token.Comment => c }

  def collectPendingWhitespaces(): Vector[Token.Whitespace] =
    pendingTokens.collect { case w: Token.Whitespace => w }

  def collectPendingTokens(): Vector[Token.Comment | Token.Whitespace] =
    pendingTokens

  def clearPendingTokens(): LexerState =
    copy(pendingTokens = Vector.empty)

  def hasPendingTokens: Boolean = pendingTokens.nonEmpty

  def hasPendingComments: Boolean =
    pendingTokens.exists(_.isInstanceOf[Token.Comment])

  def hasPendingWhitespaces: Boolean =
    pendingTokens.exists(_.isInstanceOf[Token.Whitespace])

  // Helper method to create a state with modified context
  def withNewLineTermination(enabled: Boolean): LexerState =
    if (this.newLineAfterBlockMeansEnds == enabled) this
    else copy(newLineAfterBlockMeansEnds = enabled)

  override def toString: String =
    t"LexerState(index=$index, current=$current, previousToken=$previousToken, remaining=${tokens.length - index} tokens)"
}

object LexerV2 {
  var DEBUG = false // Keep DEBUG flag for tests that use it
  private val MAX_LIST_ELEMENTS = 50 // Constants for parser configuration
}

import LexerV2.DEBUG

class LexerV2(initState: LexerState, source: Source, ignoreLocation: Boolean) {

  var state: LexerState = initState

  private def debug(msg: => String): Unit = if (DEBUG) println(t"[DEBUG] $msg")

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
          t"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
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
          ExprMeta(Some(SourcePos(source, RangeInFile(start.range.start, end.range.end))), None)
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
  private def buildOpSeq(terms: Vector[Expr])(leadingComments: Vector[CommOrWhite]): Either[ParseError, Expr] = {
    debug(t"Building OpSeq with terms: $terms")
    terms match {
      case Vector() => Left(ParseError("Empty operator sequence", getStartPos(this.state.current)))
      case Vector(expr) if leadingComments.nonEmpty =>
        Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), leadingComments))))
      case Vector(expr) => Right(expr)
      case _            => Right(OpSeq(terms, createMetaWithComments(None, leadingComments)))
    }
  }

  // Main expression continuation parser
  private def parseRest(expr: Expr)(leadingComments: Vector[CommOrWhite]): Either[ParseError, Expr] = {
    var localTerms = Vector(expr)
    debug(t"parseRest called with expr: $expr, state: ${this.state}, current terms: $localTerms")

    // Handle special closing brace + newline pattern
    if (checkForRBraceNewlinePattern()) {
      debug("parseRest: Terminating expression due to }\n pattern")
      // State is already set correctly
      return buildOpSeq(localTerms)(leadingComments)
    }

    // Handle comments and check for terminators
    val (_restComments, current) = collectComments()
    // Set state to current to check for terminators
    this.state = current
    if (isAtTerminator()) {
      debug("parseRest: Hit terminator token")
      // this.state is already set to current
      return buildOpSeq(localTerms)(leadingComments)
    }

    // Main token dispatch
    this.state.current match {
      // Match expression handling - treat like any other expression
      case Right(Token.Identifier(chars, _)) if charsToString(chars) == "match" && expr.isInstanceOf[ConcreteIdentifier] =>
        debug("parseRest: Found match keyword after identifier")
        val matchId = ConcreteIdentifier("match", createMeta(None, None))
        // Advance past the match keyword
        this.state = this.state.advance()

        // For match blocks, parse using the regular block parser with no special case handling
        // this.state is already updated
        parseBlock().map { block =>
          // Create the match expression with the block as-is
          OpSeq(Vector(expr, matchId, block), None)
        }

      // Block argument handling
      case Right(Token.LBrace(braceSourcePos)) =>
        debug("parseRest: Found LBrace after expression, treating as block argument")
        // this.state is already set to the current state
        handleBlockArgument(expr, localTerms, braceSourcePos)(leadingComments)

      // Colon handling (type annotations, etc)
      case Right(Token.Colon(sourcePos)) =>
        debug("parseRest: Found colon")
        // this.state is already set to the current state
        handleColon(sourcePos, localTerms)(leadingComments)

      // Dot call handling
      case Right(Token.Dot(dotSourcePos)) =>
        debug("parseRest: Found dot")
        // this.state is already set to the current state
        handleDotCall(dotSourcePos, localTerms).flatMap { dotCall =>
          localTerms = Vector(dotCall)
          debug(t"parseRest: After dot call, terms: $localTerms")
          // handleDotCall has updated this.state already
          parseRest(dotCall)(leadingComments)
        }

      // Operator handling
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseRest: Found operator $op")
        // this.state is already set to the current state
        handleOperatorInRest(op, sourcePos, localTerms)(leadingComments)

      // Identifier handling
      case Right(Token.Identifier(chars, sourcePos)) =>
        val text = charsToString(chars)
        debug(t"parseRest: Found identifier $text")
        // this.state is already set to the current state
        handleIdentifierInRest(text, sourcePos, localTerms)(leadingComments)

      // Generic token handling
      case Right(_) =>
        debug("parseRest: Found other token, parsing as atom")
        // this.state is already set to the current state
        withComments(() => parseAtom()).flatMap { next =>
          localTerms = localTerms :+ next
          debug(t"parseRest: After parsing other token as atom, terms: $localTerms")
          // withComments has updated this.state already
          parseRest(next)(leadingComments).map { result =>
            result match {
              case opSeq: OpSeq =>
                OpSeq(localTerms.dropRight(1) ++ opSeq.seq, None)
              case _ =>
                OpSeq(localTerms, None)
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
  private def handleBlockArgument(expr: Expr, terms: Vector[Expr], braceSourcePos: SourcePos)(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] =
    // this.state is already set correctly
    parseBlock().flatMap { block =>
      // parseBlock has already updated this.state
      // Create appropriate expression based on context
      val newExpr = expr match {
        case funcCall: FunctionCall =>
          debug("parseRest: Adding block as argument to existing function call")
          FunctionCall(
            funcCall,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
          )
        case id: ConcreteIdentifier =>
          debug("parseRest: Creating function call with block argument from identifier")
          FunctionCall(
            id,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
          )
        case _ =>
          debug("parseRest: Default handling for block after expression")
          block
      }

      debug(t"handleBlockArgument: Created expression $newExpr")

      // Handle function calls directly
      if (newExpr.isInstanceOf[FunctionCall]) {
        debug("parseRest: Returning function call with block directly")

        if (this.state.isAtTerminator) {
          Right(newExpr)
        } else {
          parseRest(newExpr)(leadingComments)
        }
      } else {
        // Handle other expressions via OpSeq
        val updatedTerms = terms.dropRight(1) :+ newExpr
        debug(t"parseRest: After handling block argument, terms: $updatedTerms")

        parseRest(newExpr)(leadingComments).map { result =>
          result match {
            case opSeq: OpSeq =>
              OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
            case _ =>
              OpSeq(updatedTerms, None)
          }
        }
      }
    }

  // Colon handling
  private def handleColon(sourcePos: SourcePos, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
    // Advance past the colon
    this.state = this.state.advance()
    val updatedTerms = terms :+ ConcreteIdentifier(":", createMeta(Some(sourcePos), Some(sourcePos)))
    debug(t"parseRest: After adding colon, terms: $updatedTerms")

    withComments(() => parseAtom()).flatMap { next =>
      val newTerms = updatedTerms :+ next
      debug(t"parseRest: After parsing atom after colon, terms: $newTerms")

      // withComments has updated this.state already
      parseRest(next)(leadingComments).map { result =>
        result match {
          case opSeq: OpSeq =>
            OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None)
          case _ =>
            OpSeq(newTerms, None)
        }
      }
    }
  }

  // Operator handling
  private def handleOperatorInRest(op: String, sourcePos: SourcePos, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
    // Advance past the operator
    this.state = this.state.advance()

    // Add operator to terms
    val updatedTerms = terms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos)))

    // Create a regular OpSeq if we're at the end of a function call argument or similar boundary
    if (
      this.state.current match {
        case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
        case _                                            => false
      }
    ) {
      debug(t"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
      // We already have set state correctly
      buildOpSeq(updatedTerms)(leadingComments)
    } else {
      // Continue parsing the rest of the expression
      withComments(() => parseAtom()).flatMap { next =>
        debug(t"parseRest: After parsing atom after operator, got: $next")
        val newTerms = updatedTerms :+ next
        debug(t"parseRest: Updated terms after operator: $newTerms")

        // Continue parsing the rest - withComments has updated this.state already
        parseRest(next)(leadingComments).map { result =>
          result match {
            case opSeq: OpSeq =>
              OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None)
            case _ =>
              OpSeq(newTerms, None)
          }
        }
      }
    }
  }

  // Identifier handling
  private def handleIdentifierInRest(text: String, sourcePos: SourcePos, terms: Vector[Expr])(
      leadingComments: Vector[CommOrWhite]
  ): Either[ParseError, Expr] = {
    // Advance past the identifier
    this.state = this.state.advance()

    this.state.current match {
      case Right(Token.LParen(_)) =>
        debug("parseRest: Found lparen after identifier")
        // this.state is already set to the position after the identifier
        parseTuple().flatMap { tuple =>
          // parseTuple has updated this.state
          val functionCall = FunctionCall(
            ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos))),
            tuple,
            createMeta(Some(sourcePos), Some(sourcePos))
          )
          val updatedTerms = terms :+ functionCall
          debug(t"parseRest: After function call, terms: $updatedTerms")

          parseRest(functionCall)(leadingComments).map { result =>
            result match {
              case opSeq: OpSeq =>
                OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
              case _ =>
                OpSeq(updatedTerms, None)
            }
          }
        }
      case Right(Token.LBrace(_)) =>
        debug("parseRest: Found lbrace after identifier")
        // this.state is already set to the position after the identifier
        parseBlock().flatMap { block =>
          // parseBlock has updated this.state
          // In V1 parser, a block after an identifier in infix is treated as part of the OpSeq
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
          val updatedTerms = terms :+ id :+ block
          debug(t"parseRest: After block in infix, terms: $updatedTerms")

          parseRest(block)(leadingComments).map { result =>
            result match {
              case opSeq: OpSeq =>
                OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
              case _ =>
                OpSeq(updatedTerms, None)
            }
          }
        }
      case Right(Token.Operator(op, opSourcePos)) =>
        debug(t"parseRest: Found operator $op after identifier")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), Some(opSourcePos)))
        val updatedTerms = terms :+ id :+ opId
        debug(t"parseRest: After adding id and op, terms: $updatedTerms")

        // Advance past the operator
        this.state = this.state.advance()
        withComments(() => parseAtom()).flatMap { next =>
          val newTerms = updatedTerms :+ next
          debug(t"parseRest: After parsing atom after operator, terms: $newTerms")

          // withComments has updated this.state already
          parseRest(next)(leadingComments).map { result =>
            result match {
              case opSeq: OpSeq =>
                OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None)
              case _ =>
                OpSeq(newTerms, None)
            }
          }
        }
      case _ =>
        debug(t"parseRest: Found bare identifier $text")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), Some(sourcePos)))
        val updatedTerms = terms :+ id
        debug(t"parseRest: After adding bare id, terms: $updatedTerms")

        parseRest(id)(leadingComments).map { result =>
          result match {
            case opSeq: OpSeq =>
              OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
            case _ =>
              OpSeq(updatedTerms, None)
          }
        }
    }
  }

  // Main parsing methods
  def parseExpr(): Either[ParseError, Expr] = {
    // Collect leading comments and initialize terms vector
    val (leadingComments, afterComments) = collectComments()
    var terms = Vector.empty[Expr]
    // Update the state to point after comments
    this.state = afterComments
    debug(t"Starting parseExpr with state: ${this.state}")

    // Main parsing logic - handle different token types
    this.state.current match {
      // Prefix operator
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseExpr: Starting with operator $op")
        // Advance past the operator
        this.state = this.state.advance()
        
        this.state.current match {
          // Function call form: op(args)
          case Right(Token.LParen(_)) =>
            debug("parseExpr: Found lparen after initial operator")
            // this.state is already pointing to the open paren
            parseTuple().map { tuple =>
              // parseTuple has already updated this.state
              (
                FunctionCall(
                  ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))),
                  tuple,
                  createMeta(Some(sourcePos), Some(sourcePos))
                )
              )
            }
          // Prefix form: op expr
          case _ =>
            debug("parseExpr: Parsing atom after initial operator")
            terms = Vector(ConcreteIdentifier(op, createMeta(Some(sourcePos), Some(sourcePos))))
            withComments(() => parseAtom()).flatMap { expr =>
              terms = terms :+ expr
              debug(t"parseExpr: After initial operator and atom, terms: $terms")

              if (this.state.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                // this.state is already updated by withComments
                Right((OpSeq(terms, None)))
              } else {
                // withComments has updated this.state already
                parseRest(expr)(leadingComments)
              }
            }
        }

      // Keyword operator handling
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) =>
        debug(t"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        // Advance past the keyword operator
        this.state = this.state.advance()
        terms = Vector(ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), Some(sourcePos))))
        withComments(() => parseAtom()).flatMap { expr =>
          terms = terms :+ expr
          debug(t"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (this.state.isAtTerminator) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            // this.state is already updated by withComments
            Right((OpSeq(terms, None)))
          } else {
            // withComments has updated this.state already
            parseRest(expr)(leadingComments)
          }
        }

      // Standard expression handling
      case _ =>
        debug("parseExpr: Starting with atom")
        withComments(() => parseAtom()).flatMap { first =>
          debug(t"parseExpr: After initial atom, got: $first")
          // withComments has updated this.state already
          parseRest(first)(leadingComments)
        }
    }
  }

  /** Checks for the pattern of a right brace followed by a newline. This is used to detect block termination in certain contexts.
    */
  private def checkForRBraceNewlinePattern(): Boolean = {
    // First check the preconditions - must be in context where newlines are significant,
    // and previous token must be a closing brace
    val originalState = this.state
    val stateWithoutComments = this.state.skipComments
    if (!stateWithoutComments.newLineAfterBlockMeansEnds || !stateWithoutComments.previousNonCommentToken.exists(_.isInstanceOf[Token.RBrace])) {
      return false
    }

    // Check if current token contains a newline or is EOF
    val hasNewline = stateWithoutComments.current match {
      case Right(ws: Token.Whitespace) =>
        lazy val wsContent = for {
          source <- source.readContent.toOption
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
    // Skip the dot
    this.state = this.state.advance()
    
    this.state.current match {
      case Right(Token.Identifier(chars1, idSourcePos1)) =>
        // Save identifier and advance
        val field = createIdentifier(chars1, idSourcePos1)
        this.state = this.state.advance()
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(): Either[ParseError, Expr] =
          this.state.current match {
            case Right(Token.LParen(_)) =>
              parseTuple().flatMap { args =>
                telescope = telescope :+ args
                parseNextTelescope()
              }
            case Right(Token.LBrace(_)) =>
              parseBlock().flatMap { block =>
                telescope = telescope :+ Tuple(Vector(block), None)
                parseNextTelescope()
              }
            case Right(Token.Dot(nextDotSourcePos)) =>
              val dotCall = createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(this.state.sourcePos))
              handleDotCall(nextDotSourcePos, Vector(dotCall))
            case _ =>
              val result = createDotCall(terms.last, field, telescope, Some(dotSourcePos), Some(this.state.sourcePos))
              Right(result)
          }

        parseNextTelescope()
      case Right(Token.Operator(op, idSourcePos)) =>
        // Save operator, advance, and process
        val field = ConcreteIdentifier(op, createMeta(Some(idSourcePos), Some(idSourcePos)))
        this.state = this.state.advance()
        
        this.state.current match {
          case Right(Token.LParen(_)) =>
            // this.state is already at the correct position
            parseTuple().map { args =>
              // parseTuple has updated this.state
              createDotCall(terms.last, field, Vector(args), Some(dotSourcePos), Some(this.state.sourcePos))
            }
          case _ =>
            Right(createDotCall(terms.last, field, Vector.empty, Some(dotSourcePos), Some(idSourcePos)))
        }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseAtom(): Either[ParseError, Expr] = {
    // Save original state for error handling
    val originalState = this.state
    
    this.state.current match {
      case Left(err) => Left(err)
      case LBrace(_) =>
        // Check for empty object or block
        val advance1 = this.state.advance().skipComments
        advance1.current match {
          case Left(err)                        => Left(err)
          case RBrace(_) => 
            // Use the original state
            parseObject() // Empty object
          case Id(_, _) | Sym(_, _) | Str(_, _) =>
            // Look ahead for = or => to determine if it's an object
            val afterId = advance1.advance().skipComments
            afterId.current match {
              case Left(err)                            => Left(err)
              case Op(op, _) if op == "=" || op == "=>" => 
                // Reset state to original
                this.state = originalState
                parseObject()
              case _                                    => 
                // Reset state to original
                this.state = originalState
                parseBlock()
            }
          case _ => 
            // Use the original state
            parseBlock()
        }

      case LParen(_) => 
        // this.state is already set to current
        parseTuple()

      case Id(chars, sourcePos) =>
        val afterId = this.state.advance()
        afterId.current match {
          case LBracket(_) =>
            // Generic type parameters
            val identifier = createIdentifier(chars, sourcePos)
            this.state = afterId
            parseList().flatMap { typeParams =>
              val afterTypeParams = this.state
              afterTypeParams.current match {
                case LParen(_) =>
                  // Function call with generic type args
                  this.state = afterTypeParams
                  parseTuple().map { tuple =>
                    val typeParamsList: ListExpr = typeParams
                    val typeCall = createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos))
                    createFunctionCall(typeCall, tuple, Some(sourcePos), Some(this.state.sourcePos))
                  }
                case _ =>
                  // Just the generic type parameters
                  val typeParamsList: ListExpr = typeParams
                  Right(
                    createFunctionCallWithTypeParams(identifier, typeParamsList, Some(sourcePos), Some(afterTypeParams.sourcePos))
                  )
              }
            }
          case LParen(_) =>
            // Regular function call
            val identifier = createIdentifier(chars, sourcePos)
            this.state = afterId
            parseFunctionCallWithId(identifier)
          case _ =>
            // Plain identifier
            this.state = afterId
            Right(createIdentifier(chars, sourcePos))
        }

      case Int(_, _) => 
        // this.state is already set to current
        parseInt()
      case Rat(_, _) => 
        // this.state is already set to current
        parseRational()
      case Str(_, _) => 
        // this.state is already set to current
        parseString()
      case Sym(_, _) => 
        // this.state is already set to current
        parseSymbol()

      case LBracket(_) => 
        // this.state is already set to current
        parseList()

      case Right(token) =>
        // Restore original state on error
        this.state = originalState
        Left(ParseError(t"Unexpected token: $token", token.sourcePos.range.start))

      case Err(error) => 
        // Restore original state on error
        this.state = originalState
        Left(error)
    }
  }

  // Helper method to check if a token is a terminator (right delimiter or comma/semicolon)
  private def isTerminator(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket | _: Token.Comma | _: Token.Semicolon | _: Token.EOF => true
    case _                                                                                                          => false
  }

  // Helper to check if current state has a terminator
  private def isAtTerminator(): Boolean = this.state.current match {
    case Right(token) => isTerminator(token)
    case _            => false
  }

  // Helper method to check if a token is specifically a right delimiter
  private def isRightDelimiter(token: Token): Boolean = token match {
    case _: Token.RParen | _: Token.RBrace | _: Token.RBracket => true
    case _                                                     => false
  }

  def parseExprList(): Either[ParseError, Vector[Expr]] = {
    // Start with current state and collect any comments
    val (_leadingListComments, initialState) = collectComments()
    this.state = initialState

    @tailrec
    def parseElements(exprs: Vector[Expr], maxExprs: Int): Either[ParseError, Vector[Expr]] =
      if (exprs.length >= maxExprs) {
        Left(ParseError(t"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", this.state.sourcePos.range.start))
      } else {
        debug(t"Iteration ${exprs.length + 1}: maxExprs=$maxExprs, current token=${this.state.current}")
        this.state.current match {
          case Right(token) if isRightDelimiter(token) =>
            debug("Found right delimiter after expression")
            Right(exprs)
          case Right(_: Token.Comment | _: Token.Whitespace) =>
            // Collect comments instead of skipping
            val (_, afterComments) = collectComments()
            this.state = afterComments
            parseElements(exprs, maxExprs)
          case Right(_: Token.Comma | _: Token.Semicolon) =>
            debug("Found comma or semicolon, skipping")
            // Collect any comments after comma/semicolon
            this.state = this.state.advance()
            val (_, afterDelimiter) = collectComments()
            this.state = afterDelimiter
            parseElements(exprs, maxExprs)
          case _ =>
            debug("Parsing expression")
            parseExpr() match {
              case Left(err) => Left(err)
              case Right(expr) =>
                // Collect comments after the expression
                val (trailingComments, afterComments) = collectComments()
                this.state = afterComments

                // Check if we've reached a terminator
                this.state.current match {
                  case Right(token) if isRightDelimiter(token) =>
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
                    Right(exprs :+ updatedExpr)

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
                    this.state = this.state.advance()
                    val (_, afterDelimiter) = collectComments()
                    this.state = afterDelimiter
                    parseElements(exprs :+ updatedExpr, maxExprs)

                  case _ =>
                    // We haven't reached a terminator, treat this as a parsing error
                    Left(ParseError("Expected delimiter after expression", this.state.sourcePos.range.start))
                }
            }
        }
      }

    parseElements(Vector.empty, LexerV2.MAX_LIST_ELEMENTS)
  }

  private def parseTuple(): Either[ParseError, Tuple] = this.state.current match {
    case LParen(sourcePos) =>
      // Advance past the left parenthesis and collect comments
      this.state = this.state.advance()
      val (leadingComments, afterLParen) = collectComments()
      this.state = afterLParen
      
      // Parse the expression list
      parseExprList().flatMap { exprs =>
        // Collect comments after the expressions
        val (trailingComments, afterList) = collectComments()
        this.state = afterList
        
        // Check for the closing parenthesis
        this.state.current match {
          case RParen(_) =>
            val meta =
              if leadingComments.nonEmpty || trailingComments.nonEmpty then
                createMeta(Some(sourcePos), Some(this.state.sourcePos))
                  .map(m =>
                    ExprMeta(
                      m.sourcePos,
                      createCommentInfo(leadingComments.collect { case c: Comment => c }, trailingComments.collect { case c: Comment => c })
                    )
                  )
              else createMeta(Some(sourcePos), Some(this.state.sourcePos))
            
            // Advance past the right parenthesis
            this.state = this.state.advance()
            Right(Tuple(exprs, meta))
            
          case _ => Left(expectedError("right parenthesis", this.state.current))
        }
      }
    case _ => Left(expectedError("left parenthesis", this.state.current))
  }

  // Check if a token is a 'case' identifier
  private def isCaseIdentifier(token: Either[ParseError, Token]): Boolean = token match {
    case Right(Token.Identifier(chars, _)) => charsToString(chars) == "case"
    case _                                 => false
  }

  private def parseBlock(): Either[ParseError, Block] = {
    // Enable newLineAfterBlockMeansEnds for all blocks
    this.state = this.state.withNewLineTermination(true)
    debug(t"parseBlock: starting with state=${this.state}")

    // Collect comments and set the state
    val (_, afterComments) = collectComments()
    this.state = afterComments
    
    var statements = Vector[Expr]()
    var result: Option[Expr] = None
    var maxExpressions = 100 // Prevent infinite loops

    // Skip the opening brace
    this.state.current match {
      case Right(Token.LBrace(_)) =>
        debug("parseBlock: Found opening brace")
        // Advance past opening brace and collect comments
        this.state = this.state.advance()
        val (_blockStartComments, afterBrace) = collectComments()
        
        // Update state to point after the brace and comments
        this.state = afterBrace
        debug(t"parseBlock: After opening brace, state=${this.state}")

        // Regular block parsing - all statements are treated the same
        while (maxExpressions > 0) {
          maxExpressions -= 1

          val (blockComments, withoutComments) = collectComments()
          this.state = withoutComments

          this.state.current match {
            case Right(Token.RBrace(_)) =>
              debug(t"parseBlock: Found closing brace, statements=${statements.size}, has result=${result.isDefined}")
              val finalBlock = Block(statements, result, None)
              this.state = this.state.advance()
              debug(t"parseBlock: Returning block with ${statements.size} statements, result=${result.isDefined}")
              return Right(finalBlock)

            case Right(Token.Semicolon(_)) =>
              debug("parseBlock: Found semicolon, advancing")
              this.state = this.state.advance()
              val (_, afterSemi) = collectComments()
              this.state = afterSemi
              debug(t"parseBlock: After semicolon, state=${this.state}")

            case Right(Token.Whitespace(_, _)) =>
              debug("parseBlock: Found whitespace, advancing")
              this.state = this.state.advance()
              val (_, afterWs) = collectComments()
              this.state = afterWs
              debug(t"parseBlock: After whitespace, state=${this.state}")

            case _ if isCaseIdentifier(this.state.current) =>
              // When we encounter 'case' at the beginning of an expression, we need to parse it
              // normally but ensure it's a separate statement
              debug("parseBlock: Found 'case' identifier, parsing statement")
              // this.state is already correctly set
              parseExpr() match {
                case Left(err) =>
                  debug(t"parseBlock: Error parsing case expression: $err")
                  return Left(err)
                case Right(expr) =>
                  // this.state has been updated by parseExpr
                  debug(t"parseBlock: Parsed case statement: $expr")
                  statements = statements :+ expr

                  // Skip past semicolons
                  this.state.current match {
                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Skipping semicolon after case statement")
                      this.state = this.state.advance()
                    case _ => 
                      // Keep current state
                  }

                  debug("parseBlock: Updated block state after case statement")
              }

            case _ =>
              debug(t"parseBlock: Parsing expression at token ${this.state.current}")
              // this.state is already correctly set
              parseExpr() match {
                case Left(err) =>
                  debug(t"parseBlock: Error parsing expression: $err")
                  return Left(err)
                case Right(expr) =>
                  // this.state has been updated by parseExpr
                  debug(t"parseBlock: Parsed expression: $expr, next token: ${this.state.current}")

                  this.state.current match {
                    case Right(Token.RBrace(_)) =>
                      debug("parseBlock: Expression followed by closing brace, setting as result")
                      // This is the V1 style: put the last expression in the result field
                      result = Some(expr)
                      this.state = this.state.advance()
                      debug(t"parseBlock: Returning block with ${statements.size} statements and result=$expr")
                      return Right(Block(statements, result, None))

                    case Right(Token.Semicolon(_)) =>
                      debug("parseBlock: Expression followed by semicolon, adding to statements")
                      statements = statements :+ expr

                      // Check for case after semicolon
                      this.state = this.state.advance()
                      val (_, afterSemi) = collectComments()

                      this.state = afterSemi
                      debug(t"parseBlock: After semicolon, statements=${statements.size}, state=${this.state}")

                    case Right(_ @ Token.Whitespace(_, _)) =>
                      debug("parseBlock: Expression followed by whitespace, checking for newlines and case")

                      // Get next token after whitespace
                      val (_, afterWs) = collectComments()

                      // Add statement and advance
                      statements = statements :+ expr
                      this.state = afterWs
                      debug(t"parseBlock: After whitespace, statements=${statements.size}, state=${this.state}")

                    case Right(_) if isCaseIdentifier(this.state.current) =>
                      debug("parseBlock: Found 'case' after expression, adding to statements")
                      statements = statements :+ expr
                      // this.state is already correctly set

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
        Left(ParseError("Too many expressions in block", this.state.sourcePos.range.start))
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
    val maybeSource = source.readContent.toOption
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
    // Save original state for error handling if needed
    val originalState = this.state
    
    // Collect comments before the object
    val (leadingComments, afterComments) = collectComments()
    this.state = afterComments

    this.state.current match {
      case Right(Token.LBrace(sourcePos)) =>
        // Collect comments after the opening brace
        this.state = this.state.advance()
        val (afterBraceComments, afterBrace) = collectComments()
        this.state = afterBrace

        def parseFields(clauses: Vector[ObjectClause]): Either[ParseError, Vector[ObjectClause]] =
          this.state.current match {
            case Right(Token.RBrace(_)) =>
              Right(clauses)
            case Right(Token.Identifier(chars, idSourcePos)) =>
              val identifier = ConcreteIdentifier(charsToString(chars), createMeta(Some(idSourcePos), Some(idSourcePos)))
              // Advance past the identifier and skip comments
              this.state = this.state.advance().skipComments
              parseField(identifier, idSourcePos).flatMap { clause =>
                checkAfterField().flatMap(_ => parseFields(clauses :+ clause))
              }
            case Right(Token.StringLiteral(chars, strSourcePos)) =>
              val stringLiteral = ConcreteStringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
              // Advance past the string literal and skip comments
              this.state = this.state.advance().skipComments
              parseField(stringLiteral, strSourcePos).flatMap { clause =>
                checkAfterField().flatMap(_ => parseFields(clauses :+ clause))
              }
            case Right(Token.SymbolLiteral(value, symSourcePos)) =>
              val symbolLiteral = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
              // Advance past the symbol literal and skip comments
              this.state = this.state.advance().skipComments
              parseField(symbolLiteral, symSourcePos).flatMap { clause =>
                checkAfterField().flatMap(_ => parseFields(clauses :+ clause))
              }
            case Right(t) =>
              Left(ParseError("Expected identifier, string literal, symbol literal or '}' in object", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }

        def parseField(key: Expr, keySourcePos: SourcePos): Either[ParseError, ObjectClause] =
          this.state.current match {
            case Right(Token.Operator(op, _)) =>
              // Advance past the operator
              this.state = this.state.advance()
              parseExpr().flatMap { value =>
                if (op == "=>") {
                  Right(ObjectExprClauseOnValue(key, value))
                } else { // op == "="
                  // For string literals with "=", convert to identifier
                  key match {
                    case stringLit: ConcreteStringLiteral =>
                      val idKey = ConcreteIdentifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                      Right(ObjectExprClause(idKey, value))
                    case qualifiedName: QualifiedName =>
                      Right(ObjectExprClause(qualifiedName, value))
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

        def checkAfterField(): Either[ParseError, Unit] =
          this.state.current match {
            case Right(Token.Comma(_)) =>
              // Collect comments after comma
              this.state = this.state.advance()
              val (_, afterComma) = collectComments()
              this.state = afterComma
              Right(())
            case Right(Token.RBrace(_)) =>
              // Keep the state as is for the closing brace
              Right(())
            case Right(t) =>
              Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
            case Left(err) =>
              Left(err)
          }

        parseFields(Vector.empty).flatMap { clauses =>
          this.state.current match {
            case Right(Token.RBrace(endPos)) =>
              // Create meta with comments
              val objectMeta = if (leadingComments.nonEmpty || afterBraceComments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBraceComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }
              
              // Advance past the closing brace
              this.state = this.state.advance()
              Right(ObjectExpr(clauses, objectMeta))
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

  private def parseList(): Either[ParseError, ListExpr] = {
    // Save original state for error handling if needed
    val originalState = this.state
    
    // Collect comments before the list
    val (leadingComments, afterComments) = collectComments()
    this.state = afterComments

    this.state.current match {
      case LBracket(sourcePos) =>
        this.state = this.state.advance()
        val (afterBracketComments, afterBracket) = collectComments()
        this.state = afterBracket

        @tailrec
        def parseElements(exprs: Vector[Expr]): Either[ParseError, Vector[Expr]] =
          if (exprs.length >= LexerV2.MAX_LIST_ELEMENTS) {
            Left(ParseError(t"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", sourcePos.range.start))
          } else
            this.state.current match {
              case RBracket(_) => Right(exprs)
              case Comma(_) =>
                this.state = this.state.advance()
                val (_, afterComma) = collectComments()
                this.state = afterComma
                parseElements(exprs)
              case Right(Token.Comment(_, _)) | Right(Token.Whitespace(_, _)) =>
                val (_, afterComments) = collectComments()
                this.state = afterComments
                parseElements(exprs)
              case _ =>
                // We're already in the right state (this.state), so parse the expression
                parseExpr() match {
                  case Left(err) => Left(err)
                  case Right(expr) =>
                    // Collect comments after the expression
                    val (_, afterComments) = collectComments()
                    this.state = afterComments
                    
                    // Check what follows the expression
                    this.state.current match {
                      case RBracket(_) => Right(exprs :+ expr)
                      case Comma(_) =>
                        this.state = this.state.advance()
                        val (_, afterComma) = collectComments()
                        this.state = afterComma
                        parseElements(exprs :+ expr)
                      case _ => Left(expectedError("',' or ']' in list", this.state.current))
                    }
                }
            }

        parseElements(Vector.empty).flatMap { exprs =>
          this.state.current match {
            case RBracket(endPos) =>
              val listMeta = if (leadingComments.nonEmpty || afterBracketComments.nonEmpty) {
                createMeta(Some(sourcePos), Some(endPos))
                  .map(m => ExprMeta(m.sourcePos, createCommentInfo(leadingComments ++ afterBracketComments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }
              
              // Advance past the closing bracket
              this.state = this.state.advance()
              Right(ListExpr(exprs, listMeta))
            case _ => Left(expectedError("']' at end of list", this.state.current))
          }
        }
      case _ => Left(expectedError("[", this.state.current))
    }
  }

  type CommOrWhite = Comment | Token.Whitespace

  extension (s: LexerState) {
    @deprecated("please handle it correctly")
    def skipComments: LexerState = {
      val savedState = LexerV2.this.state
      LexerV2.this.state = s
      val result = collectComments()._2
      LexerV2.this.state = savedState
      result
    }
  }

  /** Collects comments from the current state. Returns a tuple of (collected comments, updated state).
    */
  private def collectComments(): (Vector[CommOrWhite], LexerState) = {
    // Save original state
    val originalState = this.state
    
    // First check if we have any pending tokens from the state
    val pendingTokens = this.state.pendingTokens
    // Update state to clear pending tokens if needed
    this.state = if (pendingTokens.nonEmpty) this.state.clearPendingTokens() else this.state

    @tailrec
    def collectRec(comments: Vector[CommOrWhite]): (Vector[CommOrWhite], LexerState) =
      if (!this.state.isAtEnd && this.state.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])) {
        this.state.current match {
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
            // Advance state before recursive call
            this.state = this.state.advance()
            collectRec(comments :+ comment)
          case Right(wt @ Token.Whitespace(_, _)) =>
            // Advance state before recursive call
            this.state = this.state.advance()
            collectRec(comments :+ wt)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        // Return the comments and current state without updating this.state
        (comments, this.state)
      }

    // Combine any pending tokens with collected tokens
    val (collectedComments, finalState) = collectRec(Vector.empty)
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

    // We're returning finalState but not updating this.state here
    // because this method is called from other methods that need to use
    // the return value directly for further processing
    (allComments, finalState)
  }

  /** Collects trailing comments after an expression until a newline or non-comment token.
    */
  private def collectTrailingComments(): (Vector[Comment], LexerState) = {
    // Save original state
    val originalState = this.state
    
    // For trailing comments, we only collect comments that appear on the same line
    // (until we hit a newline in whitespace)
    @tailrec
    def collectRec(
        comments: Vector[Comment],
        hitNewline: Boolean
    ): (Vector[Comment], LexerState) =
      if (
        !this.state.isAtEnd && !hitNewline &&
        this.state.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])
      ) {
        this.state.current match {
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
            // Advance state before recursive call
            this.state = this.state.advance()
            collectRec(comments :+ comment, hitNewline)
          case Right(Token.Whitespace(_, _)) =>
            // In Whitespace tokens, we don't have the actual text content
            // Just assume any whitespace might contain a newline and stop collecting
            // Advance state before recursive call
            this.state = this.state.advance()
            collectRec(comments, true)
          case _ =>
            throw new RuntimeException("Unreachable: exists check guarantees we have a Comment or Whitespace token")
        }
      } else {
        // Return the comments and current state without updating this.state
        (comments, this.state)
      }

    // We're returning the result but not updating this.state here
    // because this method is called from other methods that need to use
    // the return value directly for further processing
    collectRec(Vector.empty, false)
  }

  /** Creates ExprMeta with comments.
    */
  private def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[CommOrWhite] = Vector.empty,
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[ExprMeta] =
    ExprMeta.maybe(sourcePos, createCommentInfo(leadingComments, trailingComments))

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
      identifier: ConcreteIdentifier
  ): Either[ParseError, FunctionCall] =
    this.state.current match {
      case LParen(_) =>
        parseTuple().map { args =>
          val funcSourcePos = identifier.meta.flatMap(_.sourcePos)
          createFunctionCall(identifier, args, funcSourcePos, Some(this.state.sourcePos))
        }
      case _ =>
        Left(ParseError("Expected left parenthesis for function call", this.state.sourcePos.range.start))
    }

  /** Generic parser combinator that adds comment handling to any parse method */
  // New overload for methods that return just an expression using mutable state
  private def withComments[T <: Expr](
      parseMethod: () => Either[ParseError, T]
  ): Either[ParseError, T] = {
    // Save original state
    val originalState = this.state
    
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments()
    
    // Update state to after leading comments
    this.state = afterLeadingComments

    // Parse the expression using the provided method
    val result = parseMethod().flatMap { expr =>
      // Collect trailing comments (state is already updated by parseMethod)
      val (trailingComments, finalState) = collectTrailingComments()
      
      // Update state to final state after trailing comments
      this.state = finalState

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
      Right(updatedExpr.asInstanceOf[T])
    }
    
    // Restore original state in case of error
    result match {
      case Left(err) => 
        this.state = originalState
        Left(err)
      case right => right
    }
  }

  // Original implementation for methods that return state in tuple format
  private def withComments[T <: Expr](
      parseMethod: LexerState => Either[ParseError, (T, LexerState)]
  ): Either[ParseError, T] = {
    // Implementation for methods that return state in tuple format
    // Save original state
    val originalState = this.state
    
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments()
    
    // Update state to after leading comments
    this.state = afterLeadingComments

    // Parse the expression using the provided method
    val result = parseMethod(this.state).flatMap { case (expr, afterExpr) =>
      // Update state to after expression
      this.state = afterExpr
      
      // Collect trailing comments
      val (trailingComments, finalState) = collectTrailingComments()
      
      // Update state to final state after trailing comments
      this.state = finalState

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
      Right(updatedExpr.asInstanceOf[T])
    }
    
    // Restore original state in case of error
    result match {
      case Left(err) => 
        this.state = originalState
        Left(err)
      case right => right
    }
  }

  private def createCommentInfo(
      leadingComments: Vector[CommOrWhite],
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[CommentInfo] =
    CommentInfo.maybe(
      commentBefore = leadingComments.collect { case c: Comment => c },
      commentInBegin = Vector.empty,
      commentInEnd = Vector.empty,
      commentEndInThisLine = trailingComments
    )

  // Helper for handling common error patterns
  private def withErrorHandling[T](
      parser: => Either[ParseError, T],
      errorMsg: String
  ): Either[ParseError, T] =
    parser match {
      case Left(err) => Left(ParseError(t"$errorMsg: ${err.message}", err.pos))
      case right     => right
    }

  // Helper that combines collecting comments and parsing with error handling
  private def withCommentsAndErrorHandling[T <: Expr](
      parser: => Either[ParseError, T],
      errorMsg: String
  ): Either[ParseError, T] = {
    // Save original state
    val originalState = this.state
    
    // Collect leading comments
    val (leadingComments, afterLeadingComments) = collectComments()
    
    // Update state to after leading comments
    this.state = afterLeadingComments

    val result = withErrorHandling(parser, errorMsg).flatMap { expr =>
      // Collect trailing comments
      val (trailingComments, finalState) = collectTrailingComments()
      
      // Update state to final state after trailing comments
      this.state = finalState

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

      Right(updatedExpr.asInstanceOf[T])
    }
    
    // Restore original state in case of error
    result match {
      case Left(err) => 
        this.state = originalState
        Left(err)
      case right => right
    }
  }

  private def parseString(): Either[ParseError, Expr] = {
    withCommentsAndErrorHandling(
      parseLiteral(
        token =>
          PartialFunction.condOpt(token) { case Token.StringLiteral(chars, sourcePos) =>
            (charsToString(chars), sourcePos)
          },
        (value, meta) => ConcreteStringLiteral(value, meta),
        "Expected string literal"
      ),
      "Error parsing string"
    )
  }

  // Create a helper method for parsing literals with common pattern
  private def parseLiteral[T <: Expr](
      extract: Token => Option[(String, SourcePos)],
      create: (String, Option[ExprMeta]) => T,
      errorMsg: String
  ): Either[ParseError, T] =
    this.state.current match {
      case Right(token) =>
        extract(token) match {
          case Some((value, sourcePos)) =>
            val meta = createMeta(Some(sourcePos), Some(sourcePos))
            // Advance the state after extracting the token
            this.state = this.state.advance()
            Right(create(value, meta))
          case None =>
            Left(ParseError(errorMsg, this.state.sourcePos.range.start))
        }
      case Left(err) => Left(err)
    }

  // Helper functions for other literal types
  private def parseInt(): Either[ParseError, Expr] = {
    withCommentsAndErrorHandling(
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
      ),
      "Error parsing integer"
    )
  }

  private def parseRational(): Either[ParseError, Expr] = {
    withCommentsAndErrorHandling(
      parseLiteral(
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
    )
  }

  private def parseSymbol(): Either[ParseError, Expr] = {
    withCommentsAndErrorHandling(
      parseLiteral(
        token =>
          PartialFunction.condOpt(token) { case Token.SymbolLiteral(value, sourcePos) =>
            (value, sourcePos)
          },
        chester.syntax.concrete.SymbolLiteral.apply,
        "Expected symbol literal"
      ),
      "Error parsing symbol"
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
