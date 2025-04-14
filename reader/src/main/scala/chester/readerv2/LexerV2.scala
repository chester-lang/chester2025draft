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

import scala.annotation.{tailrec, unused}

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
  def apply(sourceOffset: SourceOffset, ignoreLocation: Boolean = false) =
    new LexerV2(LexerState(sourceOffset, Vector.empty, 0), ignoreLocation)

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

import LexerV2.DEBUG

class LexerV2(initState: LexerState, ignoreLocation: Boolean ) {
  import LexerV2._
  var stateVar: LexerState = initState

  private inline def state: LexerState = stateVar
  private inline def execute[T](s: LexerState=>(T, LexerState)): (T, LexerState) = {
    val result = s(state)
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
      case Vector(expr) =>
        // Instead of destructuring directly, get the values separately
        val pendingTokensState = state.collectPending
        val pendingComments: Vector[CommOrWhite] = pendingTokensState._1.map {
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
        }
        stateVar = pendingTokensState._2
        Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), pendingComments))))
      case _ =>
        // Normalize the terms into a flattened sequence instead of nested OpSeqs
        val flattenedTerms = flattenOpSeq(terms)
        
        // Instead of destructuring directly, get the values separately
        val pendingTokensState = state.collectPending
        val pendingComments: Vector[CommOrWhite] = pendingTokensState._1.map {
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
        }
        stateVar = pendingTokensState._2
        Right(OpSeq(flattenedTerms, createMetaWithComments(None, pendingComments)))
    }
  }

  // Flattens nested OpSeq instances to create a single level sequence
  private def flattenOpSeq(terms: Vector[Expr]): Vector[Expr] = {
    terms.flatMap {
      case OpSeq(seq, _) => flattenOpSeq(seq)
      case other => Vector(other)
    }
  }

  // Main expression continuation parser
  def parseRest(expr: Expr): Either[ParseError, Expr] = {
    var localTerms = Vector(expr)

    // Handle special closing brace + newline pattern
    if (checkForRBraceNewlinePattern()) {
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
        withComments(parseBlock0()).map { block =>
          // Create the match expression with the block as-is
          val matchExpr = OpSeq(Vector(expr, matchId, block), None)
          matchExpr
        }

      // Block argument handling
      case Right(Token.LBrace(braceSourcePos)) if expr.isInstanceOf[DotCall] =>
        debug("parseRest: Found LBrace after dot call, treating as additional argument")
        val dotCall = expr.asInstanceOf[DotCall]
        
        withComments(parseBlock0()).map { block =>
          // Add the block as an additional telescope to the DotCall
          // Create a Tuple with the block as its only element
          val blockTuple = Tuple(Vector(block), createMeta(Some(braceSourcePos), None))
          DotCall(
            dotCall.expr, 
            dotCall.field, 
            dotCall.telescope :+ blockTuple,
            dotCall.meta
          )
        }
        
      // Block argument handling (regular)
      case Right(Token.LBrace(braceSourcePos)) =>
        debug("parseRest: Found LBrace after expression, treating as block argument")
        handleBlockArgument(expr, localTerms, braceSourcePos)

      // Colon handling (type annotations, etc)
      case Right(Token.Colon(sourcePos)) =>
        debug("parseRest: Found colon")
        handleColon(sourcePos, localTerms)

      // Dot call handling
      case Right(Token.Dot(dotSourcePos)) =>
        debug("parseRest: Found dot")
        handleDotCall(dotSourcePos, localTerms).flatMap { dotCall =>
          localTerms = Vector(dotCall)
          debug(t"parseRest: After dot call, terms: $localTerms")
          parseRest(dotCall)
        }

      // Operator handling
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseRest: Found operator $op")
        handleOperatorInRest(op, sourcePos, localTerms)

      // Identifier handling
      case Right(Token.Identifier(chars, sourcePos)) =>
        val text = charsToString(chars)
        debug(t"parseRest: Found identifier $text")
        handleIdentifierInRest(text, sourcePos, localTerms)
        
      // Tuple after a DotCall - additional argument list
      case Right(Token.LParen(sourcePos)) if expr.isInstanceOf[DotCall] =>
        debug("parseRest: Found LParen after dot call, adding argument list")
        val dotCall = expr.asInstanceOf[DotCall]
        
        withComments(parseTuple0()).map { tuple =>
          // Add the tuple as an additional telescope to the DotCall
          DotCall(
            dotCall.expr, 
            dotCall.field, 
            dotCall.telescope :+ tuple,
            dotCall.meta
          )
        }

      // Generic token handling
      case Right(_) =>
        debug("parseRest: Found other token, parsing as atom")
        withComments(parseAtom0()).flatMap { next =>
          localTerms = localTerms :+ next
          debug(t"parseRest: After parsing other token as atom, terms: $localTerms")
          parseRest(next).map { result =>
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
  private def handleBlockArgument(
    expr: Expr,
    localTerms: Vector[Expr],
    braceSourcePos: SourcePos
  ): Either[ParseError, Expr] = {
    debug("handleBlockArgument")
    
    withComments(parseBlock0()).flatMap { block =>
      debug(t"handleBlockArgument: Got block $block")
      val newTerms = localTerms :+ block
      // Check if we should terminate or continue parsing
      if (isAtTerminator(state)) {
        debug("handleBlockArgument: At terminator after block")
        buildOpSeq(newTerms)
      } else {
        debug("handleBlockArgument: Not at terminator after block, continuing")
        parseRest(OpSeq(newTerms, None))
      }
    }
  }

  // Colon handling
  private def handleColon(
    sourcePos: SourcePos,
    localTerms: Vector[Expr]
  ): Either[ParseError, Expr] = {
    debug("handleColon")
    
    d(_.advance().skip())
    // Parse the type expression after the colon
    withComments(parseAtom0()).flatMap { typeExpr =>
      debug(t"handleColon: Got type expr $typeExpr")
      val colonExpr = ConcreteIdentifier(":", createMeta(Some(sourcePos), None))
      val newTerms = localTerms :+ colonExpr :+ typeExpr
      
      if (isAtTerminator(state)) {
        debug("handleColon: At terminator after type")
        buildOpSeq(newTerms)
      } else {
        debug("handleColon: Not at terminator after type, continuing")
        parseRest(OpSeq(newTerms, None))
      }
    }
  }

  // Operator handling
  def handleOperatorInRest(op: String, sourcePos: SourcePos, localTerms: Vector[Expr]): Either[ParseError, Expr] = {
    d(_.advance())

    // Add operator to terms
    val updatedTerms = localTerms :+ ConcreteIdentifier(op, createMeta(Some(sourcePos), None))

    // Create a regular OpSeq if we're at the end of a function call argument or similar boundary
    if (state.current match {
        case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
        case _                                             => false
      }) 
    {
      debug(t"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
      buildOpSeq(updatedTerms)
    } else {
      // Continue parsing the rest of the expression
      withComments(parseAtom0()).flatMap { next =>
        debug(t"parseRest: After parsing atom after operator, got: $next")
        val newTerms = updatedTerms :+ next
        debug(t"parseRest: Updated terms after operator: $newTerms")

        // Continue parsing the rest - use OpSeq to maintain the operator expression
        if (isAtTerminator(state)) {
          buildOpSeq(newTerms)
        } else {
          parseRest(OpSeq(newTerms, None))
        }
      }
    }
  }

  // Identifier handling
  def handleIdentifierInRest(text: String, sourcePos: SourcePos, localTerms: Vector[Expr]): Either[ParseError, Expr] = {
    d(_.advance())

    state.current match {
      case Right(Token.LParen(_)) =>
        debug("parseRest: Found lparen after identifier")
        withComments(parseTuple0()).flatMap { tuple =>
          val functionCall = FunctionCall(
            ConcreteIdentifier(text, createMeta(Some(sourcePos), None)),
            tuple,
            createMeta(Some(sourcePos), None)
          )
          debug(t"parseRest: After function call, got: $functionCall")
          parseRest(functionCall)
        }
      case Right(Token.LBrace(_)) =>
        debug("parseRest: Found lbrace after identifier")
        withComments(parseBlock0()).flatMap { block =>
          // In V1 parser, a block after an identifier in infix is treated as part of the OpSeq
          val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), None))
          val updatedTerms = localTerms :+ id :+ block
          debug(t"parseRest: After block in infix, terms: $updatedTerms")

          // Continue parsing with the updated terms
          if (isAtTerminator(state)) {
            buildOpSeq(updatedTerms)
          } else {
            parseRest(OpSeq(updatedTerms, None))
          }
        }
      case Right(Token.Operator(op, opSourcePos)) =>
        debug(t"parseRest: Found operator $op after identifier")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), None))
        val opId = ConcreteIdentifier(op, createMeta(Some(opSourcePos), None))
        val updatedTerms = localTerms :+ id :+ opId
        debug(t"parseRest: After adding id and op, terms: $updatedTerms")

        d(_.advance())
        withComments(parseAtom0()).flatMap { next =>
          val newTerms = updatedTerms :+ next
          debug(t"parseRest: After parsing atom after operator, terms: $newTerms")

          // Continue parsing with the updated terms
          if (isAtTerminator(state)) {
            buildOpSeq(newTerms)
          } else {
            parseRest(OpSeq(newTerms, None))
          }
        }
      case _ =>
        debug(t"parseRest: Found bare identifier $text")
        val id = ConcreteIdentifier(text, createMeta(Some(sourcePos), None))
        val updatedTerms = localTerms :+ id
        debug(t"parseRest: After adding bare id, terms: $updatedTerms")

        // Try parsing special keywords
        text match {
          case "val" | "var" | "def" | "case" | "if" | "then" | "else" | "match" =>
            // Handle these keywords by including them in the OpSeq
            if (isAtTerminator(state)) {
              buildOpSeq(updatedTerms)
            } else {
              parseRest(OpSeq(updatedTerms, None))
            }
          case _ =>
            // Regular identifier
            parseRest(id)
        }
    }
  }

  // Main parsing methods
  def parseExpr(): Either[ParseError, Expr] = {
    parseExpr0()
  }

  /** Checks for the pattern of a right brace followed by a newline. This is used to detect block termination in certain contexts.
    */
  def checkForRBraceNewlinePattern(): Boolean = {
    if (!state.current.exists(_.isInstanceOf[Token.RBrace])) {
      return false
    }

    val prevState = state
    val nextState = state.advance()
    val nextToken = nextState.current
    stateVar = prevState
    
    nextToken.exists(t => t.isInstanceOf[Token.Whitespace] || t.isInstanceOf[Token.EOF])
  }

  private def handleDotCall(
    dotSourcePos: SourcePos,
    localTerms: Vector[Expr]
  ): Either[ParseError, Expr] = {
    debug("handleDotCall")
    
    // Determine the target expression from localTerms
    val target = if (localTerms.length == 1) localTerms.head else OpSeq(localTerms, None)
    
    d(_.advance().skip())
    state.current match {
      case Right(Token.Identifier(chars, sourcePos)) =>
        val methodName = charsToString(chars)
        val methodId = ConcreteIdentifier(methodName, createMeta(Some(sourcePos), None))
        debug(t"handleDotCall: Got method $methodId")
        
        d(_.advance())
        
        // Check for function call with arguments
        state.current match {
          case Right(Token.LParen(_)) =>
            debug("handleDotCall: Found lparen after method, parsing arguments")
            withComments(parseTuple0()).map { args =>
              debug(t"handleDotCall: Got args $args")
              DotCall(
                target,
                methodId,
                Vector(args),
                createMeta(Some(dotSourcePos), None)
              )
            }
          case _ =>
            debug("handleDotCall: No arguments for dot call")
            Right(DotCall(
              target,
              methodId,
              Vector.empty,
              createMeta(Some(dotSourcePos), None)
            ))
        }
        
      case Right(Token.Operator(op, sourcePos)) =>
        // Handle operator as method name (like obj.+)
        val methodId = ConcreteIdentifier(op, createMeta(Some(sourcePos), None))
        debug(t"handleDotCall: Got operator method $methodId")
        
        d(_.advance())
        
        state.current match {
          case Right(Token.LParen(_)) =>
            debug("handleDotCall: Found lparen after operator method, parsing arguments")
            withComments(parseTuple0()).map { args =>
              debug(t"handleDotCall: Got args $args")
              DotCall(
                target,
                methodId,
                Vector(args),
                createMeta(Some(dotSourcePos), None)
              )
            }
          case _ =>
            debug("handleDotCall: No arguments for dot call")
            Right(DotCall(
              target,
              methodId,
              Vector.empty,
              createMeta(Some(dotSourcePos), None)
            ))
        }
        
      case _ =>
        withComments(parseAtom0()).map { methodExpr =>
          debug(t"handleDotCall: Got non-identifier method $methodExpr")
          
          methodExpr match {
            case id: ConcreteIdentifier =>
              DotCall(
                target,
                id,
                Vector.empty,
                createMeta(Some(dotSourcePos), None)
              )
            case _ =>
              // For non-identifier methods, fall back to creating an OpSeq
              val dotExpr = ConcreteIdentifier(".", createMeta(Some(dotSourcePos), None))
              OpSeq(localTerms :+ dotExpr :+ methodExpr, None)
          }
        }
    }
  }

  // Parse an atom (a basic expression unit)
  private def parseAtom(): Either[ParseError, Expr] = {
    skip() // Skip leading whitespace
    parseAtom0()
  }

  // Skip to the helper method to look ahead in the token stream without advancing state
  private def skipAhead(steps: Int): Either[ParseError, Token] = {
    if (steps <= 0) return state.current
    
    var idx = state.index
    var token = state.tokens.lift(idx)
    var stepsLeft = steps
    
    while (stepsLeft > 0 && token.isDefined) {
      idx += 1
      token = state.tokens.lift(idx)
      
      // Skip comments and whitespace
      token match {
        case Some(Right(_: Token.Comment | _: Token.Whitespace)) => 
          // Don't decrement stepsLeft for whitespace/comments
        case _ => stepsLeft -= 1
      }
    }
    
    token.getOrElse(Right(Token.EOF(state.sourcePos)))
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

  /** Parse a list of expressions separated by delimiters. */
  def parseExprList(): Either[ParseError, Vector[Expr]] = {
    parseExprList0()
  }

  /** Parse a complete tuple. */
  def parseTuple(): Either[ParseError, Tuple] = state.current match {
    case LParen(sourcePos) =>
      d(_.advance().skip())
      
      if (state.current match { case RParen(_) => true; case _ => false }) {
        // Empty tuple
        val endPos = state.sourcePos
        d(_.advance())
        Right(Tuple(Vector.empty, createMeta(Some(sourcePos), Some(endPos))))
      } else {
        // Parse expressions until right paren
        parseExprList().flatMap { exprs =>
          state.current match {
            case Right(Token.RParen(endPos)) =>
              d(_.advance())
              Right(Tuple(exprs, createMeta(Some(sourcePos), Some(endPos))))
          case _ =>
              Left(ParseError("Expected right parenthesis at end of tuple", state.sourcePos.range.start))
          }
        }
      }
    case _ => 
      Left(ParseError("Expected left parenthesis at start of tuple", state.sourcePos.range.start))
  }

  /** Parse a complete list. */
  def parseList(): Either[ParseError, ListExpr] = state.current match {
    case LBracket(sourcePos) =>
      d(_.advance().skip())
      
      if (state.current match { case RBracket(_) => true; case _ => false }) {
        // Empty list
        val endPos = state.sourcePos
        d(_.advance())
        Right(ListExpr(Vector.empty, createMeta(Some(sourcePos), Some(endPos))))
      } else {
        // Parse expressions until right bracket
        parseExprList().flatMap { exprs =>
          state.current match {
            case Right(Token.RBracket(endPos)) =>
              d(_.advance())
              Right(ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))))
          case _ =>
              Left(ParseError("Expected right bracket at end of list", state.sourcePos.range.start))
          }
        }
      }
    case _ => 
      Left(ParseError("Expected left bracket at start of list", state.sourcePos.range.start))
  }

  /** Parse a complete block. */
  def parseBlock(): Either[ParseError, Block] = {
    // Enable newLineAfterBlockMeansEnds for all blocks
    d(_.withNewLineTermination(true))
    debug(t"parseBlock: starting with state=$state")

    state.current match {
      case Right(Token.LBrace(startPos)) =>
        debug("parseBlock: Found opening brace")
        d(_.advance().skip())
        debug(t"parseBlock: After opening brace, state=$state")

        // Parse statements until right brace
        var stmts = Vector.empty[Expr]
        var result: Option[Expr] = None
        var maxExpressions = 100 // Prevent infinite loops

        @tailrec
        def parseStatements(): Either[ParseError, Vector[Expr]] = 
          if (stmts.length >= maxExpressions) {
            Left(ParseError("Too many expressions in block", state.sourcePos.range.start))
          } else {
            state.current match {
              case Right(Token.RBrace(_)) => 
                Right(stmts)
              case Right(_) =>
                parseExpr0() match {
                  case Left(err) => Left(err)
                  case Right(expr) =>
                    stmts = stmts :+ expr
                    if (stmts.size == 1) {
                      result = Some(expr)
                    }
                    
                    // Skip whitespace/comments after expression
                    d(_.skip())
                    parseStatements()
                }
              case Left(err) => Left(err)
            }
          }

        parseStatements().flatMap { finalStatements =>
          // Create the block expression with the collected statements
          state.current match {
            case Right(Token.RBrace(endPos)) =>
              debug(t"parseBlock: End of block, statements=${finalStatements.size}")
              val blockMeta = createMeta(Some(startPos), Some(endPos))
              d(_.advance())
              if (finalStatements.isEmpty) {
                Right(Block(Vector.empty, None, blockMeta))
              } else {
                val lastExpr = finalStatements.last
                Right(Block(finalStatements.dropRight(1), Some(lastExpr), blockMeta))
              }
            case _ => 
              Left(ParseError("Expected right brace at end of block", state.sourcePos.range.start))
          }
        }
      case _ => 
        Left(ParseError("Expected left brace at start of block", state.sourcePos.range.start))
    }
  }

  // Helper method to check if whitespace contains a newline
  private def isNewlineWhitespace(token: Token.Whitespace): Boolean = {
    val maybeSource = state.source.readContent.toOption
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
    val startState = if (pendingTokens.nonEmpty) state.clearPendingTokens else state
    
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

  /** Collects trailing comments after an expression until a newline or non-comment token. */
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
        current.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])
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

    collectRec(state, Vector.empty, false)
  }

  /** Creates ExprMeta with comments. */
  private def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[CommOrWhite] = Vector.empty,
      trailingComments: Vector[Comment] = Vector.empty
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
        // Save the current state
        val savedState = stateVar
        // Set the state to the passed state
        stateVar = state
        parseTuple0().map { args =>
          val afterArgs = stateVar
          // Restore the current state
          stateVar = savedState
          val funcSourcePos = identifier.meta.flatMap(_.sourcePos)
          (createFunctionCall(identifier, args, funcSourcePos, Some(afterArgs.sourcePos)), afterArgs)
        }
      case _ =>
        Left(ParseError("Expected left parenthesis for function call", state.sourcePos.range.start))
    }

  /** Generic wrapper for withComments that works with the state-based approach */
  private def withComments[T <: Expr](parseMethod: => Either[ParseError, T]): Either[ParseError, T] = {
    // Collect leading comments without destructuring
    val pendingTokensState = state.collectPending
    val leadingComments: Vector[CommOrWhite] = pendingTokensState._1.map {
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
    }
    stateVar = pendingTokensState._2

    // Parse the expression using the provided method
    parseMethod.map { expr =>
      // Collect trailing comments without destructuring
      val trailingWithState = collectTrailingComments(state)
      val trailingComments = trailingWithState._1
      stateVar = trailingWithState._2

      // Update expression with comments
      if (!leadingComments.isEmpty || !trailingComments.isEmpty) {
        // Need to cast back to T since expr.updateMeta returns Expr
        val updatedExpr = expr.updateMeta { existingMeta =>
          val newMeta = createMetaWithComments(
            existingMeta.flatMap(_.sourcePos),
            leadingComments,
            trailingComments
          )

          // Merge the existing meta with new comment information
          mergeMeta(existingMeta, newMeta)
        }
        updatedExpr.asInstanceOf[T]
      } else {
        expr
      }
    }
  }

  /** Creates CommentInfo from comments. */
  private def createCommentInfo(
      leadingComments: Vector[CommOrWhite] = Vector.empty,
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[CommentInfo] = {
    val leadingActualComments = leadingComments.collect { case c: Comment => c }
    
    if (leadingActualComments.isEmpty && trailingComments.isEmpty) {
      None
    } else {
      Some(CommentInfo(
        commentBefore = leadingActualComments, 
        commentInBegin = Vector.empty,
        commentInEnd = Vector.empty,
        commentEndInThisLine = trailingComments
      ))
    }
    }

  // Helper for handling common error patterns
  private def withErrorHandling[T](
      parser: LexerState => Either[ParseError, (T, LexerState)],
      errorMsg: String
  ): LexerState => Either[ParseError, (T, LexerState)] = state =>
    parser(state) match {
      case Left(err) => Left(ParseError(t"$errorMsg: ${err.message}", err.pos))
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
            PartialFunction.condOpt(token) { case Token.StringLiteral(chars, sourcePos) =>
              (charsToString(chars), sourcePos)
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

  // Public methods for external callers
  
  /** Parse a full expression and return it. */
  def parseExpr0(): Either[ParseError, Expr] = {
    // Collect leading comments and initialize terms vector
    val result = execute(_.collectPending)
    val leadingComments = result._1
    stateVar = result._2
    
    var terms = Vector.empty[Expr]
    debug(t"Starting parseExpr with state: $state")

    // Main parsing logic - handle different token types
    state.current match {
      // Prefix operator
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseExpr: Starting with operator $op")
        d(_.advance().skip())
        state.current match {
          // Function call form: op(args)
          case Right(Token.LParen(_)) =>
            debug("parseExpr: Found lparen after initial operator")
            withComments(parseTuple0()).map { tuple =>
              FunctionCall(
                ConcreteIdentifier(op, createMeta(Some(sourcePos), None)),
                tuple,
                createMeta(Some(sourcePos), None)
              )
            }
          // Prefix form: op expr
          case _ =>
            debug("parseExpr: Parsing atom after initial operator")
            terms = Vector(ConcreteIdentifier(op, createMeta(Some(sourcePos), None)))
            withComments(parseAtom0()).flatMap { expr =>
              terms = terms :+ expr
              debug(t"parseExpr: After initial operator and atom, terms: $terms")

              if (isAtTerminator(state)) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                Right(OpSeq(terms, None))
              } else {
                parseRest(OpSeq(terms, None))
              }
            }
        }

      // Keyword operator handling
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) =>
        debug(t"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        d(_.advance().skip())
        terms = Vector(ConcreteIdentifier(charsToString(chars), createMeta(Some(sourcePos), None)))
        withComments(parseAtom0()).flatMap { expr =>
          terms = terms :+ expr
          debug(t"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (isAtTerminator(state)) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            Right(OpSeq(terms, None))
          } else {
            parseRest(OpSeq(terms, None))
          }
        }
        
      // Special keyword handling (val, def, case, etc.)
      case Right(Token.Identifier(chars, sourcePos)) =>
        val text = charsToString(chars)
        debug(t"parseExpr: Starting with identifier $text")
        
        // Handle all identifiers uniformly
        debug("parseExpr: Starting with atom")
        withComments(parseAtom0()).flatMap { first =>
          debug(t"parseExpr: After initial atom, got: $first")
          parseRest(first)
        }

      // Integer literal handling (for cases like "1 and 5")
      case Right(Token.IntegerLiteral(value, sourcePos)) =>
        debug(t"parseExpr: Starting with integer literal $value")
        // Parse the integer
        withComments(parseAtom0()).flatMap { intExpr =>
          debug(t"parseExpr: After parsing integer, got: $intExpr")
          parseRest(intExpr)
        }

      // Standard expression handling
      case _ =>
        debug("parseExpr: Starting with atom")
        withComments(parseAtom0()).flatMap { first =>
          debug(t"parseExpr: After initial atom, got: $first")
          parseRest(first)
        }
    }
  }
  
  /** Internal expression list parser implementation */
  private def parseExprList0(): Either[ParseError, Vector[Expr]] = {
    @tailrec
    def parseElements(exprs: Vector[Expr], maxExprs: Int): Either[ParseError, Vector[Expr]] =
      if (exprs.length >= maxExprs) {
        Left(ParseError(t"Too many elements in list (maximum is ${LexerV2.MAX_LIST_ELEMENTS})", state.sourcePos.range.start))
      } else {
        state.current match {
          case Right(token) if isRightDelimiter(token) =>
            // End of list
            Right(exprs)
          case Right(_: Token.Comment | _: Token.Whitespace) =>
            // Skip comments and whitespace
            d(_.advance())
            parseElements(exprs, maxExprs)
          case Right(_: Token.Comma | _: Token.Semicolon) =>
            // Skip delimiters
            d(_.advance().skip())
            parseElements(exprs, maxExprs)
          case _ =>
            // Parse expression
            parseExpr0() match {
              case Left(err) => Left(err)
              case Right(expr) =>
                val newExprs = exprs :+ expr
                d(_.skip())
                
                // Check for delimiter
                state.current match {
                  case Right(token) if isRightDelimiter(token) => 
                    Right(newExprs)
                  case Right(_: Token.Comma | _: Token.Semicolon) =>
                    d(_.advance().skip())
                    parseElements(newExprs, maxExprs)
                  case _ =>
                    Left(ParseError("Expected delimiter after expression", state.sourcePos.range.start))
                }
            }
        }
      }

    parseElements(Vector.empty, LexerV2.MAX_LIST_ELEMENTS)
  }
  
  /** Parse a complete tuple (internal implementation) */
  private def parseTuple0(): Either[ParseError, Tuple] = state.current match {
    case LParen(sourcePos) =>
      d(_.advance().skip())
      
      if (state.current match { case RParen(_) => true; case _ => false }) {
        // Empty tuple
        val endPos = state.sourcePos
        d(_.advance())
        Right(Tuple(Vector.empty, createMeta(Some(sourcePos), Some(endPos))))
      } else {
        // Parse expressions until right paren
        parseExprList0().flatMap { exprs =>
          state.current match {
            case Right(Token.RParen(endPos)) =>
              d(_.advance())
              Right(Tuple(exprs, createMeta(Some(sourcePos), Some(endPos))))
            case _ => 
              Left(ParseError("Expected right parenthesis at end of tuple", state.sourcePos.range.start))
          }
        }
      }
    case _ => 
      Left(ParseError("Expected left parenthesis at start of tuple", state.sourcePos.range.start))
  }
  
  /** Parse a complete list (internal implementation) */
  private def parseList0(): Either[ParseError, ListExpr] = state.current match {
    case LBracket(sourcePos) =>
      d(_.advance().skip())
      
      if (state.current match { case RBracket(_) => true; case _ => false }) {
        // Empty list
        val endPos = state.sourcePos
        d(_.advance())
        Right(ListExpr(Vector.empty, createMeta(Some(sourcePos), Some(endPos))))
      } else {
        // Parse expressions until right bracket
        parseExprList0().flatMap { exprs =>
          state.current match {
            case Right(Token.RBracket(endPos)) =>
              d(_.advance())
              Right(ListExpr(exprs, createMeta(Some(sourcePos), Some(endPos))))
            case _ => 
              Left(ParseError("Expected right bracket at end of list", state.sourcePos.range.start))
          }
        }
      }
    case _ => 
      Left(ParseError("Expected left bracket at start of list", state.sourcePos.range.start))
  }
  
  /** Parse an atom (internal implementation) */
  private def parseAtom0(): Either[ParseError, Expr] = state.current match {
    case Left(err) => Left(err)
    case LBrace(_) =>
      // Need to determine if it's an object expression or a block
      val originalState = stateVar
      
      // Empty object case
      if (skipAhead(1) match { case Right(Token.RBrace(_)) => true; case _ => false }) {
        debug("parseAtom0: Found empty object")
        d(_.advance().advance()) // Skip the { and }
        val startPos = originalState.sourcePos
        val endPos = state.sourcePos
        return Right(ObjectExpr(Vector.empty, createMeta(Some(startPos), Some(endPos))))
      }
      
      // Try to determine if it's an object by looking ahead at the key-op pattern
      val isObject = skipAhead(1) match {
        // Look for identifier followed by = or =>
        case Right(Token.Identifier(_, _)) | Right(Token.SymbolLiteral(_, _)) | Right(Token.StringLiteral(_, _)) =>
          skipAhead(2) match {
            case Right(Token.Operator(op, _)) if op == "=" || op == "=>" => true
            case _ => false
          }
        case _ => false
      }
      
      if (isObject) {
        debug("parseAtom0: Identified as object based on lookahead pattern")
        // Reset state and use parseObject which is now fixed
        stateVar = originalState
        parseObject()
      } else {
        debug("parseAtom0: Identified as block based on lookahead pattern")
        // Reset state and parse as block
        stateVar = originalState
        parseBlock0()
      }
    case LParen(_) => 
      parseTuple0()
    case LBracket(_) => 
      parseList0()
    case Id(chars, sourcePos) =>
      d(_.advance())
      
      state.current match {
        case LBracket(_) =>
          // Generic type parameters
          val identifier = createIdentifier(chars, sourcePos)
          withComments(parseList0()).flatMap { typeParams =>
            if (state.current match { case LParen(_) => true; case _ => false }) {
                // Function call with generic type args
              withComments(parseTuple0()).map { tuple =>
                val typeCall = createFunctionCallWithTypeParams(
                  identifier, 
                  typeParams, 
                  Some(sourcePos), 
                  Some(state.sourcePos)
                )
                createFunctionCall(typeCall, tuple, Some(sourcePos), Some(state.sourcePos))
              }
            } else {
              // Just type params
              Right(createFunctionCallWithTypeParams(
                identifier, 
                typeParams, 
                Some(sourcePos), 
                Some(state.sourcePos)
              ))
            }
          }
        case LParen(_) =>
          // Function call
          val identifier = createIdentifier(chars, sourcePos)
          withComments(parseTuple0()).map { tuple =>
            createFunctionCall(
              identifier, 
              tuple, 
              Some(sourcePos), 
              Some(state.sourcePos)
            )
          }
        case _ =>
          // Plain identifier
          Right(createIdentifier(chars, sourcePos))
      }
    case Int(value, sourcePos) =>
      // Handle different bases
      val (numStr, base) =
        if (value.startsWith("0x")) (value.drop(2), 16)
        else if (value.startsWith("0b")) (value.drop(2), 2)
        else (value, 10)
      
      try {
        val intValue = BigInt(numStr, base)
        d(_.advance())
        Right(ConcreteIntegerLiteral(intValue, createMeta(Some(sourcePos), Some(sourcePos))))
      } catch {
        case _: NumberFormatException =>
          Left(ParseError("Invalid integer literal", sourcePos.range.start))
      }
    case Rat(value, sourcePos) =>
      try {
        val ratValue = spire.math.Rational(BigDecimal(value))
        d(_.advance())
        Right(ConcreteRationalLiteral(ratValue, createMeta(Some(sourcePos), Some(sourcePos))))
      } catch {
        case _: NumberFormatException =>
          Left(ParseError("Invalid rational literal", sourcePos.range.start))
      }
    case Str(chars, sourcePos) =>
      val strValue = charsToString(chars)
      d(_.advance())
      Right(ConcreteStringLiteral(strValue, createMeta(Some(sourcePos), Some(sourcePos))))
    case Sym(value, sourcePos) =>
      d(_.advance())
      Right(chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(sourcePos), Some(sourcePos))))
    case Right(token) =>
      Left(ParseError(s"Unexpected token: $token", token.sourcePos.range.start))
    case Err(error) =>
      Left(error)
  }
  
  /** Updates the state to use the given tokens */
  def setTokens(tokens: Vector[Either[ParseError, Token]]): LexerV2 = {
    stateVar = LexerState(state.source, tokens, 0)
    this
  }

  /** Parse a complete block (internal implementation). */
  private def parseBlock0(): Either[ParseError, Block] = {
    // Enable newLineAfterBlockMeansEnds for all blocks
    d(_.withNewLineTermination(true))
    debug(t"parseBlock: starting with state=$state")

    state.current match {
      case Right(Token.LBrace(startPos)) =>
        debug("parseBlock: Found opening brace")
        d(_.advance().skip())
        debug(t"parseBlock: After opening brace, state=$state")

        // Parse statements until right brace
        var stmts = Vector.empty[Expr]
        var result: Option[Expr] = None
        var maxExpressions = 100 // Prevent infinite loops

        @tailrec
        def parseStatements(): Either[ParseError, Vector[Expr]] = 
          if (stmts.length >= maxExpressions) {
            Left(ParseError("Too many expressions in block", state.sourcePos.range.start))
          } else {
            state.current match {
              case Right(Token.RBrace(_)) => 
                Right(stmts)
              case Right(_) =>
                parseExpr0() match {
                  case Left(err) => Left(err)
                  case Right(expr) =>
                    stmts = stmts :+ expr
                    if (stmts.size == 1) {
                      result = Some(expr)
                    }
                    
                    // Skip whitespace/comments after expression
                    d(_.skip())
                    parseStatements()
                }
              case Left(err) => Left(err)
            }
          }

        parseStatements().flatMap { finalStatements =>
          // Create the block expression with the collected statements
          state.current match {
            case Right(Token.RBrace(endPos)) =>
              debug(t"parseBlock: End of block, statements=${finalStatements.size}")
              val blockMeta = createMeta(Some(startPos), Some(endPos))
              d(_.advance())
              if (finalStatements.isEmpty) {
                Right(Block(Vector.empty, None, blockMeta))
              } else {
                val lastExpr = finalStatements.last
                Right(Block(finalStatements.dropRight(1), Some(lastExpr), blockMeta))
              }
            case _ => 
              Left(ParseError("Expected right brace at end of block", state.sourcePos.range.start))
          }
        }
      case _ => 
        Left(ParseError("Expected left brace at start of block", state.sourcePos.range.start))
    }
  }

  def parseExpr(state: LexerState): Either[ParseError, (Expr, LexerState)] = {
    val savedState = stateVar
    stateVar = state
    val result = parseExpr()
    val newState = stateVar
    stateVar = savedState
    result.map(expr => (expr, newState))
  }

  /**
   * Handle whitespace automatically by skipping tokens
   */
  def skip(): Unit = {
    d(_.skip())
  }

  // Method to set state directly for tests
  def setState(newState: LexerState): LexerV2 = {
    stateVar = newState
    this
  }
  
  // Method to get the current state
  def getState(): LexerState = state

  /** Initializes the lexer with tokenized input */
  def init(): Either[ParseError, Unit] = {
    val tokenizer = chester.readerv2.Tokenizer(state.source)
    val tokenStream = tokenizer.tokenize()
    
    // First check if there's an error in the first token
    tokenStream.headOption match {
      case Some(Left(error)) => Left(error)
      case _ =>
        // Convert LazyList to Vector
        val tokens = tokenStream.toVector
        
        // Filter out comment and whitespace tokens for initial parsing
        val filteredTokens = tokens.filter {
          case Right(_: Token.Comment | _: Token.Whitespace) => false
          case _ => true
        }
        stateVar = LexerState(state.source, filteredTokens, 0)
        Right(())
    }
  }
  
  /** Parse an expression using a string input */
  def parseString(input: String): Either[ParseError, Expr] = {
    val source = FileNameAndContent("input", input)
    val sourceOffset = SourceOffset(source)
    val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
    val tokenStream = tokenizer.tokenize()
    
    // First check if there's an error in the first token
    tokenStream.headOption match {
      case Some(Left(error)) => Left(error)
      case _ =>
        // Convert LazyList to Vector
        val tokens = tokenStream.toVector
        
        // Filter out whitespace tokens for parsing
        val filteredTokens = tokens.filter {
          case Right(_: Token.Whitespace) => false
          case _ => true
        }
        // Create a new lexer state with the tokens
        val newState = LexerState(sourceOffset, filteredTokens, 0)
        // Save the current state
        val savedState = stateVar
        // Set the new state
        stateVar = newState
        // Parse the expression
        val result = parseExpr()
        // Restore the original state
        stateVar = savedState
        // Return the result
        result
    }
  }

  /** Parse a complete object. */
  def parseObject(): Either[ParseError, ObjectExpr] = {
    debug("parseObject: starting")
    state.current match {
      case Right(Token.LBrace(startPos)) =>
        d(_.advance().skip())
        
        // Empty object case
        if (state.current match { case Right(Token.RBrace(_)) => true; case _ => false }) {
          debug("parseObject: Empty object")
          val endPos = state.sourcePos
          d(_.advance())
          return Right(ObjectExpr(Vector.empty, createMeta(Some(startPos), Some(endPos))))
        }
        
        // Parse key-value pairs
        var clauses = Vector.empty[ObjectClause]
        
        while (true) {
          // First check if we've reached the end of the object
          if (state.current match { case Right(Token.RBrace(_)) => true; case _ => false }) {
            val endPos = state.sourcePos
            d(_.advance())
            return Right(ObjectExpr(clauses, createMeta(Some(startPos), Some(endPos))))
          }
          
          // Parse key
          val keyResult = parseExpr0()
          keyResult match {
            case Left(err) => return Left(err)
            case Right(rawKey) =>
              // Process the key expression to handle the case where it's an OpSeq
              val key = rawKey match {
                case OpSeq(terms, _) if terms.length == 3 && terms(1).isInstanceOf[Identifier] && 
                                        (terms(1).asInstanceOf[Identifier].name == "=" || 
                                         terms(1).asInstanceOf[Identifier].name == "=>") =>
                  // This is already a key-value pair in an OpSeq form, extract the key
                  terms.head
                case _ => rawKey
              }
              
              d(_.skip())
              
              // Check for = or => operator
              state.current match {
                case Right(Token.Operator(op, _)) if op == "=" || op == "=>" =>
                  debug(s"parseObject: Found operator $op")
                  d(_.advance().skip())
                  
                  // Parse value
                  parseExpr0() match {
                    case Left(err) => return Left(err)
                    case Right(value) =>
                      debug(s"parseObject: Got value ${value}")
                      
                      // Create the appropriate clause type based on the key and operator
                      val clause = key match {
                        case id: Identifier =>
                          if (op == "=") {
                            ObjectExprClause(id, value)
                          } else { // op == "=>"
                            ObjectExprClauseOnValue(id, value)
                          }
                        case sym: SymbolLiteral =>
                          if (op == "=") {
                            // Convert symbol to identifier for = case
                            val identifier = Identifier(sym.value, sym.meta)
                            ObjectExprClause(identifier, value)
                          } else { // op == "=>"
                            ObjectExprClauseOnValue(sym, value)
                          }
                        case str: StringLiteral =>
                          if (op == "=") {
                            // Convert string to identifier for = case
                            val identifier = Identifier(str.value, str.meta)
                            ObjectExprClause(identifier, value)
                          } else { // op == "=>"
                            ObjectExprClauseOnValue(str, value)
                          }
                        case _ =>
                          // Invalid key type, but let's create a clause on value anyway
                          ObjectExprClauseOnValue(key, value)
                      }
                      
                      // Add clause to list
                      clauses = clauses :+ clause
                      
                      // Skip whitespace
                      d(_.skip())
                      
                      // Check for comma or right brace
                      state.current match {
                        case Right(Token.RBrace(endPos)) =>
                          // End of object - success
                          d(_.advance())
                          return Right(ObjectExpr(clauses, createMeta(Some(startPos), Some(endPos))))
                        
                        case Right(Token.Comma(_)) =>
                          // Skip the comma and continue parsing
                          d(_.advance().skip())
                          
                          // Allow trailing comma - check for right brace after comma
                          if (state.current match { case Right(Token.RBrace(_)) => 
                            debug("parseObject: Found trailing comma")
                            val endPos = state.sourcePos
                            d(_.advance())
                            return Right(ObjectExpr(clauses, createMeta(Some(startPos), Some(endPos))))
                            true
                          case _ => false }) {}
                          
                          // Continue loop to parse next key-value pair
                          
                        case _ =>
                          return Left(ParseError("Expected comma or right brace after object value", state.sourcePos.range.start))
                      }
                  }
                
                // Special case for when the key is already in the form of an OpSeq with = or =>
                case _ if rawKey != key =>
                  // We've already extracted the key from an OpSeq, now get the value directly
                  val opSeq = rawKey.asInstanceOf[OpSeq]
                  val op = opSeq.seq(1).asInstanceOf[Identifier].name
                  val value = opSeq.seq(2)
                  
                  // Create the appropriate clause type
                  val clause = key match {
                    case id: Identifier =>
                      if (op == "=") {
                        ObjectExprClause(id, value)
                      } else { // op == "=>"
                        ObjectExprClauseOnValue(id, value)
                      }
                    case sym: SymbolLiteral =>
                      if (op == "=") {
                        // Convert symbol to identifier for = case
                        val identifier = Identifier(sym.value, sym.meta)
                        ObjectExprClause(identifier, value)
                      } else { // op == "=>"
                        ObjectExprClauseOnValue(sym, value)
                      }
                    case str: StringLiteral =>
                      if (op == "=") {
                        // Convert string to identifier for = case
                        val identifier = Identifier(str.value, str.meta)
                        ObjectExprClause(identifier, value)
                      } else { // op == "=>"
                        ObjectExprClauseOnValue(str, value)
                      }
                    case _ =>
                      // Invalid key type, but let's create a clause on value anyway
                      ObjectExprClauseOnValue(key, value)
                  }
                  
                  // Add clause to list
                  clauses = clauses :+ clause
                  
                  // Skip whitespace
                  d(_.skip())
                  
                  // Check for comma or right brace
                  state.current match {
                    case Right(Token.RBrace(endPos)) =>
                      // End of object - success
                      d(_.advance())
                      return Right(ObjectExpr(clauses, createMeta(Some(startPos), Some(endPos))))
                    
                    case Right(Token.Comma(_)) =>
                      // Skip the comma and continue parsing
                      d(_.advance().skip())
                      
                      // Allow trailing comma - check for right brace after comma
                      if (state.current match { case Right(Token.RBrace(_)) => 
                        debug("parseObject: Found trailing comma")
                        val endPos = state.sourcePos
                        d(_.advance())
                        return Right(ObjectExpr(clauses, createMeta(Some(startPos), Some(endPos))))
                        true
                      case _ => false }) {}
                      
                      // Continue loop to parse next key-value pair
                      
                    case _ =>
                      return Left(ParseError("Expected comma or right brace after object value", state.sourcePos.range.start))
                  }
                
                case _ =>
                  return Left(ParseError("Expected = or => in object clause", state.sourcePos.range.start))
              }
          }
        }
        
        // Unreachable, but Scala needs a value here
        Left(ParseError("Unexpected error in object parsing", state.sourcePos.range.start))
        
      case _ =>
        Left(ParseError("Expected left brace at start of object", state.sourcePos.range.start))
    }
  }
}
