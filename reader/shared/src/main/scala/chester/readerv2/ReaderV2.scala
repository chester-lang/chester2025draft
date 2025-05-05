package chester.readerv2
import chester.i18n.*
import chester.error.{Pos, RangeInFile, SourcePos, unreachableOr}
import chester.reader.{ParseError, Source}
import chester.syntax.concrete.{Block, DotCall, ExprMeta, FunctionCall, ListExpr, ObjectClause, ObjectExpr, ObjectExprClause, ObjectExprClauseOnValue, OpSeq, ParsedExpr, Tuple}
import chester.reader.FileNameAndContent
import chester.syntax.IdentifierRules.strIsOperator
import chester.syntax.*
import chester.syntax.concrete.*
import chester.utils.*

import scala.annotation.tailrec

case class ReaderContext(
    inOpSeq: Boolean = false,
    dontallowOpSeq: Boolean = false,
    newLineAfterBlockMeansEnds: Boolean = false,
    dontAllowBlockApply: Boolean = false
) {
  def opSeq: Boolean = !inOpSeq && !dontallowOpSeq

  def blockCall: Boolean = !inOpSeq && !dontAllowBlockApply
}

case class ReaderState(
    tokens: Vector[Either[ParseError, Token]],
    index: Int,
    previousToken: Option[Token] = None,
    previousNonCommentToken: Option[Token] = None,
    pendingTokens: Vector[Token.Comment | Token.Whitespace] = Vector.empty
) {
  def current: Either[ParseError, Token] = tokens(index)
  def isAtEnd: Boolean = index >= tokens.length
  def advance(): ReaderState = current match {
    case Right(token: Token.Comment) =>
      copy(index = index + 1, previousToken = Some(token), pendingTokens = pendingTokens :+ token)
    case Right(token: Token.Whitespace) =>
      copy(index = index + 1, previousToken = Some(token), pendingTokens = pendingTokens :+ token)
    case Right(token) =>
      copy(index = index + 1, previousToken = Some(token), previousNonCommentToken = Some(token))
    case Left(_) =>
      copy(index = index + 1)
  }
  def sourcePos: SourcePos = current.orelse(previousToken) match {
    case Left(err) => err.sourcePos.getOrElse(SourcePos(Source(FileNameAndContent("", "")), RangeInFile(Pos.zero, Pos.zero)))
    case Right(t)  => t.sourcePos
  }

  // Helper methods for common state checks
  def isAtTerminator: Boolean = current.exists(t =>
    t.isInstanceOf[Token.EOF] || t.isInstanceOf[Token.RParen] ||
      t.isInstanceOf[Token.RBrace] || t.isInstanceOf[Token.RBracket] ||
      t.isInstanceOf[Token.Comma] || t.isInstanceOf[Token.Semicolon]
  )

  def clearPendingTokens(): ReaderState =
    copy(pendingTokens = Vector.empty)

  override def toString: String =
    t"ReaderState(index=$index, current=$current, previousToken=$previousToken, remaining=${tokens.length - index} tokens)"
}

object ReaderV2 {
  val DEBUG: Parameter[Boolean] = Parameter.withDefault(false)
}

class ReaderV2(initState: ReaderState, source: Source, ignoreLocation: Boolean) {

  var state: ReaderState = initState

  private def debug(msg: => String): Unit = if (ReaderV2.DEBUG.get) println(s"[DEBUG] $msg")

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
      case _: Token.Semicolon       => "semicolon"
      case _: Token.EOF             => "end of file"
      case _: Token.Whitespace      => "whitespace"
      case _                        => "unknown token"
    }

    token.fold(
      identity,
      t =>
        ParseError(
          f"Expected $expected but found ${getTokenType(t)} at ${t.sourcePos.range.start.line}:${t.sourcePos.range.start.column}",
          t.sourcePos.range.start
        )
    )
  }

  /** Creates expression metadata from source positions and comments. */
  private def createMeta(startPos: Option[SourcePos], endPos: Option[SourcePos]): Option[ExprMeta] =
    if (ignoreLocation) None
    else
      (startPos, endPos) match {
        case (Some(start), Some(end)) =>
          ExprMeta.maybe(Some(SourcePos(source, RangeInFile(start.range.start, end.range.end))))
        case (Some(pos), _) =>
          ExprMeta.maybe(Some(pos))
        case (_, Some(pos)) =>
          ExprMeta.maybe(Some(pos))
        case _ => None
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

  /** Helper for building operator sequences Gets comments directly from pendingTokens via pullComments()
    */
  private def buildOpSeq(terms: Vector[ParsedExpr], context: ReaderContext = ReaderContext()): Either[ParseError, ParsedExpr] = {
    debug(t"Building OpSeq with terms: $terms")
    // Get any collected comments from pendingTokens
    val comments = pullComments()

    terms match {
      case Vector() => Left(ParseError("Empty operator sequence", getStartPos(this.state.current)))
      case Vector(expr) if comments.nonEmpty =>
        Right(expr.updateMeta(meta => mergeMeta(meta, createMetaWithComments(meta.flatMap(_.sourcePos), comments))))
      case Vector(expr) => Right(expr)
      case _ =>
        debug(t"Creating OpSeq with terms: $terms")
        Right(OpSeq(terms, createMetaWithComments(None, comments)))
    }
  }

  /** Main expression continuation parser. Uses skipComments() and pullComments() internally to handle comments without passing them around.
    */
  private def parseRest(expr: ParsedExpr, context: ReaderContext = ReaderContext()): Either[ParseError, ParsedExpr] = {
    var localTerms = Vector(expr)
    debug(t"parseRest called with expr: $expr, state: ${this.state}, current terms: $localTerms")

    // Handle special closing brace + newline pattern
    if (checkForRBraceNewlinePattern(context = context)) {
      debug("parseRest: Terminating expression due to }\n pattern")
      // State is already set correctly
      // buildOpSeq will pull comments internally
      return buildOpSeq(localTerms)
    }

    // Skip comments and check for terminators
    skipComments()
    if (this.state.isAtTerminator) {
      debug("parseRest: Hit terminator token")
      // buildOpSeq will pull comments internally
      return buildOpSeq(localTerms)
    }

    // Main token dispatch
    this.state.current match {
      case Right(Token.LBrace(braceSourcePos)) =>
        debug(t"parseRest: Found LBrace after expression $expr, attempting object parse first.")
        val originalState = this.state // Save state before attempting object parse

        // Try parsing as an object
        parseObject() match {
          case Right(objExpr) =>
            debug(t"parseRest: Successfully parsed as ObjectExpr: $objExpr")
            // Check if the preceding expression allows an object argument
            expr match {
              case id: Identifier =>
                debug(t"parseRest: Preceding expr is Identifier $id, creating FunctionCall with object arg.")
                val funcCall = FunctionCall(
                  id,
                  Tuple(Vector(objExpr), createMeta(None, None)), // Wrap object in a Tuple
                  createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
                )
                // parseObject advanced state, now continue parsing after the object
                parseRest(funcCall, context = context) // Recurse with the new FunctionCall
              case funcCall: FunctionCall =>
                debug(t"parseRest: Preceding expr is FunctionCall $funcCall, adding object as another argument.")
                // This assumes the object is an additional argument group like f(a){b=1}
                val newFuncCall = FunctionCall(
                  funcCall,
                  Tuple(Vector(objExpr), createMeta(None, None)),
                  createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
                )
                // parseObject advanced state, now continue parsing after the object
                parseRest(newFuncCall, context = context) // Recurse with the new FunctionCall
              case _ =>
                debug(t"parseRest: Preceding expr $expr doesn't support object arg directly. Falling back to block parse.")
                this.state = originalState // Restore state
                handleBlockArgument(expr, localTerms, braceSourcePos) // Treat as block
            }

          case Left(_) => // Failed to parse as object
            debug("parseRest: Failed to parse as ObjectExpr, falling back to block parse.")
            this.state = originalState // Restore state before trying block parse
            handleBlockArgument(expr, localTerms, braceSourcePos, context = context) // Treat as block
        }

      // Colon handling (type annotations, etc)
      case Right(Token.Colon(sourcePos)) =>
        debug("parseRest: Found colon")
        // this.state is already set to the current state
        {
          // Advance past the colon
          advance()
          val updatedTerms = localTerms :+ Identifier(":", createMeta(Some(sourcePos), None))
          debug(t"parseRest: After adding colon, terms: $updatedTerms")

          withComments(() => parseAtom(context = context)).flatMap { next =>
            val newTerms = updatedTerms :+ next
            debug(t"parseRest: After parsing atom after colon, terms: $newTerms")

            // withComments has updated this.state already
            parseRest(next, context = context).map {
              case opSeq: OpSeq =>
                debug(t"OpSeq construction in handleColon: newTerms=$newTerms, dropping last=${newTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}")
                debug(t"Creating OpSeq with combined sequence: ${newTerms.dropRight(1) ++ opSeq.seq}")
                OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None)
              case _ =>
                debug(t"Simple OpSeq construction in handleColon: newTerms=$newTerms")
                OpSeq(newTerms, None)
            }
          }
        }

      // Dot call handling
      case Right(Token.Dot(dotSourcePos)) =>
        debug("parseRest: Found dot")
        // this.state is already set to the current state
        handleDotCall(dotSourcePos, localTerms).flatMap { dotCall =>
          localTerms = Vector(dotCall)
          debug(t"parseRest: After dot call, terms: $localTerms")
          // handleDotCall has updated this.state already
          parseRest(dotCall, context = context)
        }

      // Operator handling
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseRest: Found operator $op")
        // this.state is already set to the current state
        {
          // Advance past the operator
          advance()

          // Add operator to terms
          val updatedTerms = localTerms :+ Identifier(op, createMeta(Some(sourcePos), None))

          // Create a regular OpSeq if we're at the end of a function call argument or similar boundary
          if (
            this.state.current match {
              case Right(Token.RParen(_)) | Right(Token.Comma(_)) => true
              case _                                              => false
            }
          ) {
            debug(t"parseRest: Added operator $op at argument boundary, terms: $updatedTerms")
            // We already have set state correctly
            // buildOpSeq will pull comments internally
            buildOpSeq(updatedTerms)
          } else {
            // Continue parsing the rest of the expression
            // Change parseAtom() to parseExprInner() to handle prefix operators after infix op
            withComments(() => parseExpr(context = context)).flatMap { next =>
              debug(t"parseRest: After parsing expr after operator, got: $next")
              val newTerms = updatedTerms :+ next
              debug(t"parseRest: Updated terms after operator: $newTerms")

              // Continue parsing the rest - withComments has updated this.state already
              parseRest(next, context = context).map {
                case opSeq: OpSeq =>
                  // Flatten the nested OpSeq
                  debug(
                    t"OpSeq construction in handleOperatorInRest: newTerms=$newTerms, dropping last=${newTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                  )
                  debug(t"Creating flattened OpSeq with combined sequence: ${newTerms.dropRight(1) ++ opSeq.seq}")
                  OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None)
                case _ => // If the rest is not an OpSeq, just build normally
                  debug(t"Simple OpSeq construction in handleOperatorInRest: newTerms=$newTerms")
                  OpSeq(newTerms, None) // newTerms already contains 'next' (which is otherExpr)
              }
            }
          }
        }

      // Identifier handling
      case Right(Token.Identifier(chars, sourcePos)) =>
        val text = charsToString(chars)
        debug(t"parseRest: Found identifier $text")
        // this.state is already set to the current state
        {
          // Advance past the identifier
          advance()

          this.state.current match {
            case Right(Token.LBracket(_)) =>
              debug("parseRest: Found lbracket after identifier - handling generic type parameters")
              // Create the identifier
              val identifier = Identifier(text, createMeta(Some(sourcePos), None))

              // Parse the generic type parameters (list)
              parseList().flatMap { typeParams =>
                val afterTypeParams = this.state
                afterTypeParams.current match {
                  case Right(Token.LParen(_)) | Right(Token.LBracket(_)) =>
                    // Function call with generic type args: id[T](args)
                    debug("parseRest: Found function call with generic type parameters")
                    parseTuple().flatMap { tuple =>
                      val typeParamsList: ListExpr = typeParams
                      val typeCall = FunctionCall(
                        identifier,
                        typeParamsList,
                        createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                      )
                      val funcCall = FunctionCall(
                        typeCall,
                        tuple.asInstanceOf[Tuple],
                        createMeta(Some(sourcePos), Some(this.state.sourcePos))
                      )
                      val updatedTerms = localTerms :+ funcCall

                      parseRest(funcCall, context = context).map {
                        case opSeq: OpSeq =>
                          debug(
                            t"OpSeq construction in handleIdentifierInRest (generic function call): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                          )
                          debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                          OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
                        case _ =>
                          debug(t"Simple OpSeq construction in handleIdentifierInRest (generic function call): updatedTerms=$updatedTerms")
                          OpSeq(updatedTerms, None)
                      }
                    }
                  case Right(Token.LBrace(_)) =>
                    // Generic type parameters followed by a block: id[T]{...}
                    debug("parseRest: Found identifier with generic type parameters followed by a block")
                    val typeParamsList: ListExpr = typeParams
                    val typeCall = FunctionCall(
                      identifier,
                      typeParamsList,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )

                    parseBlock().flatMap { block =>
                      // Don't create a function call - instead just add both expressions to the sequence
                      val updatedTerms = localTerms :+ typeCall :+ block
                      debug(t"parseRest: After block following generic type, terms: $updatedTerms")

                      parseRest(block, context = context).map {
                        case opSeq: OpSeq =>
                          debug(
                            t"OpSeq construction in handleIdentifierInRest (generic type with block): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                          )
                          debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                          OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
                        case _ =>
                          debug(t"Simple OpSeq construction in handleIdentifierInRest (generic type with block): updatedTerms=$updatedTerms")
                          OpSeq(updatedTerms, None)
                      }
                    }
                  case _ =>
                    // Just generic type parameters: id[T]
                    debug("parseRest: Found identifier with generic type parameters")
                    val typeParamsList: ListExpr = typeParams
                    val funcCall = FunctionCall(
                      identifier,
                      typeParamsList,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )
                    val updatedTerms = localTerms :+ funcCall

                    parseRest(funcCall, context = context).map {
                      case opSeq: OpSeq =>
                        debug(
                          t"OpSeq construction in handleIdentifierInRest (generic type): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                        )
                        debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                        OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
                      case _ =>
                        debug(t"Simple OpSeq construction in handleIdentifierInRest (generic type): updatedTerms=$updatedTerms")
                        OpSeq(updatedTerms, None)
                    }
                }
              }

            case Right(Token.LParen(_)) =>
              debug("parseRest: Found lparen after identifier")
              // this.state is already set to the position after the identifier
              parseTuple(context0 = context).flatMap { tuple =>
                // parseTuple has updated this.state
                val functionCall = FunctionCall(
                  Identifier(text, createMeta(Some(sourcePos), None)),
                  tuple,
                  createMeta(Some(sourcePos), None)
                )
                val updatedTerms = localTerms :+ functionCall
                debug(t"parseRest: After function call, terms: $updatedTerms")

                parseRest(functionCall, context = context).map {
                  case opSeq: OpSeq =>
                    // Flatten
                    debug(
                      t"OpSeq construction in handleIdentifierInRest (function call): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                    )
                    debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                    OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
                  case _ =>
                    debug(t"Simple OpSeq construction in handleIdentifierInRest (function call): updatedTerms=$updatedTerms")
                    OpSeq(updatedTerms, None)
                }
              }

            case Right(Token.LBrace(_)) =>
              debug("parseRest: Found lbrace after identifier")
              // this.state is already set to the position after the identifier
              // Decide if block or object based on content
              val originalState = this.state
              parseObject() match {
                case Right(objExpr) => // Object argument
                  debug(t"handleIdentifierInRest: Parsed object arg $objExpr after identifier $text")
                  val idExpr = Identifier(text, createMeta(Some(sourcePos), None))
                  val funcCall =
                    FunctionCall(idExpr, Tuple(Vector(objExpr), createMeta(None, None)), createMeta(Some(sourcePos), Some(this.state.sourcePos)))
                  val updatedTerms = localTerms :+ funcCall
                  parseRest(funcCall, context = context).map {
                    case opSeq: OpSeq =>
                      debug(
                        t"OpSeq construction in handleIdentifierInRest (object): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                      )
                      debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                      OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
                    case _ =>
                      debug(t"Simple OpSeq construction in handleIdentifierInRest (object): updatedTerms=$updatedTerms")
                      OpSeq(updatedTerms, None)
                  }
                case Left(_) => // Assume block argument
                  this.state = originalState // Restore state
                  debug(t"handleIdentifierInRest: Failed object parse, assuming block arg after identifier $text")
                  parseBlock().flatMap { block =>
                    val id = Identifier(text, createMeta(Some(sourcePos), None))
                    // Change: Always treat as `id block` elements in OpSeq like V1 for compatibility
                    val updatedTerms = localTerms :+ id :+ block
                    debug(t"parseRest: After block following identifier, terms: $updatedTerms")
                    parseRest(block, context = context).map {
                      case opSeq: OpSeq =>
                        debug(
                          t"OpSeq construction in handleIdentifierInRest (block): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                        )
                        debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                        OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
                      case _ =>
                        debug(t"Simple OpSeq construction in handleIdentifierInRest (block): updatedTerms=$updatedTerms")
                        OpSeq(updatedTerms, None)
                    }
                  }
              }

            case Right(Token.Operator(op, opSourcePos)) =>
              debug(t"parseRest: Found operator $op after identifier")
              val id = Identifier(text, createMeta(Some(sourcePos), None))
              val opId = Identifier(op, createMeta(Some(opSourcePos), None))
              val updatedTerms = localTerms :+ id :+ opId
              debug(t"parseRest: After adding id and op, terms: $updatedTerms")

              // Advance past the operator
              advance()
              withComments(() => parseExpr()).flatMap { next => // Using parseExprInner now
                val newTerms = updatedTerms :+ next
                debug(t"parseRest: After parsing expr after operator, terms: $newTerms")

                // withComments has updated this.state already
                parseRest(next, context = context).map {
                  case opSeq: OpSeq =>
                    // Flatten the nested OpSeq
                    debug(
                      t"OpSeq construction in handleIdentifierInRest (operator): newTerms=$newTerms, dropping last=${newTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                    )
                    debug(t"Creating flattened OpSeq with combined sequence: ${newTerms.dropRight(1) ++ opSeq.seq}")
                    OpSeq(newTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
                  case _ =>
                    debug(t"Simple OpSeq construction in handleIdentifierInRest (operator): newTerms=$newTerms")
                    OpSeq(newTerms, None)
                }
              }
            case _ =>
              debug(t"parseRest: Found bare identifier $text")
              val id = Identifier(text, createMeta(Some(sourcePos), None))
              val updatedTerms = localTerms :+ id
              debug(t"parseRest: After adding bare id, terms: $updatedTerms")

              parseRest(id, context = context).map {
                case opSeq: OpSeq =>
                  debug(
                    t"OpSeq construction in handleIdentifierInRest (bare id): updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                  )
                  debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                  OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
                // Special case adjustment: Treat f(block) returned from block handling as f block in sequence
                case FunctionCall(f, Tuple(Vector(b: Block), _), _) =>
                  debug(t"Special case in handleIdentifierInRest: treating f(block) as sequence [f, block]")
                  debug(t"Creating OpSeq with adjusted sequence: ${updatedTerms.dropRight(1) ++ Vector(f, b)}")
                  OpSeq(updatedTerms.dropRight(1) ++ Vector(f, b), None)
                case _ =>
                  debug(t"Simple OpSeq construction in handleIdentifierInRest (bare id): updatedTerms=$updatedTerms")
                  OpSeq(updatedTerms, None)
              }
          }
        }

      // Generic token handling
      case Right(_) =>
        debug("parseRest: Found other token, parsing as atom")
        // this.state is already set to the current state
        withComments(() => parseAtom()).flatMap { next =>
          localTerms = localTerms :+ next
          debug(t"parseRest: After parsing other token as atom, terms: $localTerms")
          // withComments has updated this.state already
          parseRest(next, context = context).map {
            case opSeq: OpSeq =>
              debug(t"OpSeq construction in parseRest: localTerms=$localTerms, dropping last=${localTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}")
              debug(t"Creating OpSeq with combined sequence: ${localTerms.dropRight(1) ++ opSeq.seq}")
              OpSeq(localTerms.dropRight(1) ++ opSeq.seq, None)
            case _ =>
              debug(t"Simple OpSeq construction in parseRest: localTerms=$localTerms")
              OpSeq(localTerms, None)
          }
        }

      // Error handling
      case Left(error) =>
        debug(t"parseRest: Got error: $error")
        Left(error)
    }
  }

  /** Handle block arguments - uses skipComments() and pullComments() for comment handling
    */
  private def handleBlockArgument(
      expr: ParsedExpr,
      terms: Vector[ParsedExpr],
      braceSourcePos: SourcePos,
      context: ReaderContext = ReaderContext()
  ): Either[ParseError, ParsedExpr] =
    // this.state is already set correctly
    parseBlock(context0 = context).flatMap { block =>
      // parseBlock has already updated this.state
      // Create appropriate expression based on context
      expr match {
        case funcCall: FunctionCall =>
          // Check if this is a case like Vector[T]{...} - don't wrap block as an argument
          val isFunctionCallWithTypeParams = funcCall.function.isInstanceOf[Identifier] &&
            funcCall.telescope.isInstanceOf[ListExpr]

          if (isFunctionCallWithTypeParams) {
            debug("parseRest: Found generic function call with block, treating as separate elements")
            // Return an OpSeq with both expressions
            val updatedTerms = terms :+ block
            debug(t"parseRest: After block following generic function, terms: $updatedTerms")

            parseRest(block, context = context).map {
              case opSeq: OpSeq =>
                debug(
                  t"OpSeq construction for generic function with block: updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
                )
                debug(t"Creating flattened OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
                OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None) // Flatten
              case _ =>
                debug(t"Simple OpSeq construction for generic function with block: updatedTerms=$updatedTerms")
                OpSeq(updatedTerms, None)
            }
          } else {
            debug("parseRest: Adding block as argument to existing function call")
            val newFuncCall = FunctionCall(
              funcCall,
              Tuple(Vector(block), createMeta(None, None)),
              createMeta(Some(funcCall.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
            )

            if (this.state.isAtTerminator) {
              Right(newFuncCall)
            } else {
              parseRest(newFuncCall, context = context)
            }
          }
        case id: Identifier =>
          debug("parseRest: Creating function call with block argument from identifier")
          val funcCall = FunctionCall(
            id,
            Tuple(Vector(block), createMeta(None, None)),
            createMeta(Some(id.meta.flatMap(_.sourcePos).getOrElse(braceSourcePos)), Some(this.state.sourcePos))
          )

          if (this.state.isAtTerminator) {
            Right(funcCall)
          } else {
            parseRest(funcCall, context = context)
          }
        case _ =>
          debug("parseRest: Default handling for block after expression")
          val updatedTerms = terms :+ block
          debug(t"parseRest: After block following other expression, terms: $updatedTerms")

          parseRest(block, context = context).map {
            case opSeq: OpSeq =>
              debug(
                t"OpSeq construction in handleBlockArgument: updatedTerms=$updatedTerms, dropping last=${updatedTerms.dropRight(1)}, opSeq.seq=${opSeq.seq}"
              )
              debug(t"Creating OpSeq with combined sequence: ${updatedTerms.dropRight(1) ++ opSeq.seq}")
              OpSeq(updatedTerms.dropRight(1) ++ opSeq.seq, None)
            case _ =>
              debug(t"Simple OpSeq construction in handleBlockArgument: updatedTerms=$updatedTerms")
              OpSeq(updatedTerms, None)
          }
      }
    }

  // Main parsing methods
  def parseExpr(context: ReaderContext = ReaderContext()): Either[ParseError, ParsedExpr] = {
    // Skip comments and whitespace, storing them internally in pendingTokens
    skipComments()
    // Initialize terms vector for expressions
    var terms = Vector.empty[ParsedExpr]
    debug(t"Starting parseExpr with state: ${this.state}")

    // Main parsing logic - handle different token types
    this.state.current match {
      // Prefix operator
      case Right(Token.Operator(op, sourcePos)) =>
        debug(t"parseExpr: Starting with operator $op")
        // Advance past the operator
        advance()

        this.state.current match {
          // Function call form: op(args)
          case Right(Token.LParen(_)) =>
            debug("parseExpr: Found lparen after initial operator")
            // this.state is already pointing to the open paren
            parseTuple().map { tuple =>
              // parseTuple has already updated this.state
              FunctionCall(
                Identifier(op, createMeta(Some(sourcePos), None)),
                tuple,
                createMeta(Some(sourcePos), None)
              )
            }
          // Prefix form: op expr
          case _ =>
            debug("parseExpr: Parsing atom after initial operator")
            terms = Vector(Identifier(op, createMeta(Some(sourcePos), None)))
            withComments(() => parseAtom()).flatMap { expr =>
              terms = terms :+ expr
              debug(t"parseExpr: After initial operator and atom, terms: $terms")

              if (this.state.isAtTerminator) {
                debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
                // this.state is already updated by withComments
                debug(t"Creating OpSeq with terms: $terms")
                Right(OpSeq(terms, None))
              } else {
                // withComments has updated this.state already
                parseRest(expr, context = context)
              }
            }
        }

      // Keyword operator handling
      case Right(Token.Identifier(chars, sourcePos)) if strIsOperator(charsToString(chars)) =>
        debug(t"parseExpr: Starting with keyword operator ${charsToString(chars)}")
        // Advance past the keyword operator
        advance()
        terms = Vector(Identifier(charsToString(chars), createMeta(Some(sourcePos), None)))
        withComments(() => parseAtom()).flatMap { expr =>
          terms = terms :+ expr
          debug(t"parseExpr: After initial keyword operator and atom, terms: $terms")

          if (this.state.isAtTerminator) {
            debug("parseExpr: Found terminator after prefix operator, returning OpSeq directly")
            // this.state is already updated by withComments
            debug(t"Creating OpSeq with terms: $terms")
            Right(OpSeq(terms, None))
          } else {
            // withComments has updated this.state already
            parseRest(expr, context = context)
          }
        }

      // Standard expression handling
      case _ =>
        debug("parseExpr: Starting with atom")
        withComments(() => parseAtom(context = context)).flatMap { first =>
          debug(t"parseExpr: After initial atom, got: $first")
          // withComments has updated this.state already
          parseRest(first, context = context)
        }
    }
  }

  /** Checks for the pattern of a right brace followed by a newline. This is used to detect block termination in certain contexts.
    */
  private def checkForRBraceNewlinePattern(context: ReaderContext = ReaderContext()): Boolean = {
    // First check the preconditions - must be in context where newlines are significant,
    // and previous token must be a closing brace
    val savedState = this.state
    // Skip comments but remember the state to restore it
    skipComments()
    val stateWithoutComments = this.state
    // Restore the original state
    this.state = savedState

    if (!context.newLineAfterBlockMeansEnds || !stateWithoutComments.previousNonCommentToken.exists(_.isInstanceOf[Token.RBrace])) {
      return false
    }

    // Check if current token contains a newline or is EOF
    val hasNewline = (stateWithoutComments.current match {
      case Right(ws: Token.Whitespace) => unreachableOr(isNewlineWhitespace(ws))
      case Right(_: Token.EOF)         => true
      case Right(_: Token.Semicolon)   => true // Also treat semicolons as terminators
      // For match expressions, always treat a case keyword as a terminator
      case _ => false
    }) || (stateWithoutComments.previousToken match {
      case Some(ws: Token.Whitespace) => isNewlineWhitespace(ws)
      case _                          => false
    })

    // Log if pattern is detected and debug is enabled
    if (ReaderV2.DEBUG.get && hasNewline) debug("}\n check: pattern detected")
    hasNewline
  }

  private def handleDotCall(
      dotSourcePos: SourcePos,
      terms: Vector[ParsedExpr],
      context: ReaderContext = ReaderContext()
  ): Either[ParseError, ParsedExpr] = {
    // Skip the dot
    advance()

    this.state.current match {
      case Right(Token.Identifier(chars1, idSourcePos1)) =>
        // Save identifier and advance
        val field = Identifier(charsToString(chars1), createMeta(Some(idSourcePos1), None))
        advance()
        var telescope = Vector.empty[Tuple]

        def parseNextTelescope(): Either[ParseError, ParsedExpr] =
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
              val dotCall = DotCall(
                terms.last,
                field,
                telescope,
                createMeta(Some(dotSourcePos), Some(this.state.sourcePos))
              )
              handleDotCall(nextDotSourcePos, Vector(dotCall))
            case _ =>
              val result = DotCall(
                terms.last,
                field,
                telescope,
                createMeta(Some(dotSourcePos), Some(this.state.sourcePos))
              )
              Right(result)
          }

        parseNextTelescope()
      case Right(Token.Operator(op, idSourcePos)) =>
        // Save operator, advance, and process
        val field = Identifier(op, createMeta(Some(idSourcePos), None))
        advance()

        this.state.current match {
          case Right(Token.LParen(_)) =>
            // this.state is already at the correct position
            parseTuple().map { args =>
              // parseTuple has updated this.state
              DotCall(
                terms.last,
                field,
                Vector(args),
                createMeta(Some(dotSourcePos), Some(this.state.sourcePos))
              )
            }
          case _ =>
            Right(
              DotCall(
                terms.last,
                field,
                Vector.empty,
                createMeta(Some(dotSourcePos), Some(idSourcePos))
              )
            )
        }
      case Right(t)  => Left(ParseError("Expected identifier or operator after '.'", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }
  }

  private def parseAtom(context: ReaderContext = ReaderContext()): Either[ParseError, ParsedExpr] = {
    // Save original state for error handling
    val originalState = this.state

    this.state.current match {
      case Left(err) => Left(err)

      case Right(Token.Hash(sourcePos)) =>
        // Advance past the hash
        advance()

        // We expect an identifier to follow the hash
        this.state.current match {
          case Right(Token.Identifier(chars, idSourcePos)) =>
            val keyName = charsToString(chars)

            // Advance past the identifier
            advance()
            skipComments()

            // Check for arguments (telescope)
            var telescope = Vector.empty[MaybeTelescope]

            def parseKeywordArguments(): Either[ParseError, Vector[MaybeTelescope]] =
              // Check for tuple arguments like (1,2,3) or list arguments like [qaq]
              this.state.current match {
                case Right(Token.LParen(_)) =>
                  // Parse tuple arguments
                  parseTuple().flatMap { tupleArg =>
                    telescope = telescope :+ tupleArg
                    // Try to parse more arguments
                    parseKeywordArguments()
                  }
                case Right(Token.LBracket(_)) =>
                  // Parse list arguments
                  parseList().flatMap { listArg =>
                    telescope = telescope :+ listArg
                    // Try to parse more arguments
                    parseKeywordArguments()
                  }
                case _ =>
                  // No more arguments
                  Right(telescope)
              }

            parseKeywordArguments().map(finalTelescope => Keyword(keyName, finalTelescope, createMeta(Some(sourcePos), Some(idSourcePos))))

          case Right(token) =>
            Left(ParseError("Expected identifier after '#'", token.sourcePos.range.start))

          case Left(err) => Left(err)
        }

      case Right(Token.LBrace(braceSourcePos)) =>
        debug(t"parseAtom: Found LBrace at $braceSourcePos, attempting object parse first.")
        // Store the state BEFORE the LBrace token was encountered.
        // originalState is already available from the start of parseAtom

        // Try parsing as an object. parseObject expects to start *at* the LBrace.
        parseObject() match {
          case Right(objExpr) =>
            debug(t"parseAtom: Successfully parsed as ObjectExpr: $objExpr")
            // Object parsing consumes tokens and advances state.
            Right(objExpr) // Return the successfully parsed object

          case Left(objError) =>
            // Failed to parse as object. Restore state to BEFORE the LBrace.
            debug(s"parseAtom: Object parse failed ($objError), restoring state and trying block parse.")
            this.state = originalState // Restore the state from the beginning of parseAtom

            // Now, try parsing as a block. parseBlock also expects to start *at* the LBrace.
            parseBlock() match {
              case Right(blockExpr) =>
                debug(t"parseAtom: Successfully parsed as Block: $blockExpr")
                // Block parsing consumes tokens and advances state.
                Right(blockExpr) // Return the successfully parsed block
              case Left(blockError) =>
                // If both fail, report the block parsing error as it's the fallback.
                // Alternatively, could try to report a more generic error or the object error?
                // Reporting block error seems more consistent with fallback logic.
                debug(s"parseAtom: Block parse also failed ($blockError). Reporting block error.")
                Left(blockError)
            }
        }

      case Right(Token.LParen(_)) =>
        // this.state is already set to current
        parseTuple()

      case Right(Token.Identifier(chars, sourcePos)) =>
        val afterId = this.state.advance()
        afterId.current match {
          case Right(Token.LBracket(_)) =>
            // Generic type parameters
            val identifier = Identifier(charsToString(chars), createMeta(Some(sourcePos), None))
            this.state = afterId
            parseList().flatMap { typeParams =>
              val afterTypeParams = this.state
              afterTypeParams.current match {
                case Right(Token.LParen(_)) =>
                  // Function call with generic type args
                  this.state = afterTypeParams
                  parseTuple().map { tuple =>
                    val typeParamsList: ListExpr = typeParams
                    val typeCall = FunctionCall(
                      identifier,
                      typeParamsList,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )
                    FunctionCall(
                      typeCall,
                      tuple.asInstanceOf[Tuple],
                      createMeta(Some(sourcePos), Some(this.state.sourcePos))
                    )
                  }
                case _ =>
                  // Just the generic type parameters
                  val typeParamsList: ListExpr = typeParams
                  Right(
                    FunctionCall(
                      identifier,
                      typeParamsList,
                      createMeta(Some(sourcePos), Some(afterTypeParams.sourcePos))
                    )
                  )
              }
            }
          case Right(Token.LParen(_)) =>
            // Regular function call
            val identifier = Identifier(charsToString(chars), createMeta(Some(sourcePos), None))
            this.state = afterId
            this.state.current match {
              case Right(Token.LParen(_)) =>
                parseTuple(context0 = context).map { args =>
                  val funcSourcePos = identifier.meta.flatMap(_.sourcePos)
                  FunctionCall(
                    identifier,
                    args.asInstanceOf[Tuple],
                    createMeta(funcSourcePos, Some(this.state.sourcePos))
                  )
                }
              case _ =>
                Left(ParseError("Expected left parenthesis for function call", this.state.sourcePos.range.start))
            }
          case _ =>
            // Plain identifier
            this.state = afterId
            Right(Identifier(charsToString(chars), createMeta(Some(sourcePos), None)))
        }

      case Right(Token.IntegerLiteral(value, _)) =>
        // this.state is already set to current
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
            (value, meta) => IntegerLiteral(BigInt(value), meta),
            "Expected integer literal"
          ),
          "Error parsing integer"
        )
      case Right(Token.RationalLiteral(value, _)) =>
        // this.state is already set to current
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
            (value, meta) => RationalLiteral(spire.math.Rational(BigDecimal(value)), meta),
            "Expected rational literal"
          ),
          "Error parsing rational number"
        )
      case Right(Token.StringLiteral(_, _)) =>
        // this.state is already set to current
        withCommentsAndErrorHandling(
          parseLiteral(
            token =>
              PartialFunction.condOpt(token) { case Token.StringLiteral(chars, sourcePos) =>
                (charsToString(chars), sourcePos)
              },
            (value, meta) => StringLiteral(value, meta),
            "Expected string literal"
          ),
          "Error parsing string"
        )
      case Right(Token.SymbolLiteral(value, _)) =>
        // this.state is already set to current
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

      case Right(Token.LBracket(_)) =>
        // this.state is already set to current
        parseList()

      case Right(token) =>
        Left(ParseError(t"Unexpected token: $token", token.sourcePos.range.start))

      case Left(error) =>
        Left(error)
    }
  }

  /** Parses a sequence of comma-separated (and optionally semicolon-separated) expressions until a specific closing token is found. Handles comment
    * attachment to elements.
    *
    * @param closingTokenPredicate
    *   Predicate to identify the closing token (e.g., `_.isInstanceOf[Token.RBracket]`).
    * @param allowSemicolon
    *   If true, semicolons are treated as valid separators like commas.
    * @param startPosForError
    *   The source position of the opening delimiter, used for error messages.
    * @param contextDescription
    *   Description of the context (e.g., "list", "tuple") for error messages.
    * @return
    *   A vector of parsed expressions or a ParseError.
    */
  @tailrec
  private def parseElementSequence(
      closingTokenPredicate: Token => Boolean,
      allowSemicolon: Boolean,
      startPosForError: SourcePos,
      contextDescription: String,
      accumulatedExprs: Vector[ParsedExpr] = Vector.empty,
      context: ReaderContext = ReaderContext()
  ): Either[ParseError, Vector[ParsedExpr]] = {
    // Skip comments and whitespace before checking the token
    skipComments()

    debug(t"parseElementSequence: Iteration ${accumulatedExprs.length + 1}, current token=${this.state.current}")

    this.state.current match {
      // Check for the closing token first
      case Right(token) if closingTokenPredicate(token) =>
        debug(s"Found closing token for $contextDescription")
        Right(accumulatedExprs)

      // Handle separators (comma and optionally semicolon)
      case Right(token @ (_: Token.Comma | _: Token.Semicolon)) =>
        if (!allowSemicolon && token.isInstanceOf[Token.Semicolon]) {
          Left(ParseError(t"Semicolon not allowed as separator in $contextDescription", token.sourcePos.range.start))
        } else {
          debug(s"Found separator ($token), skipping")
          advance() // Skip the separator
          // Continue parsing elements recursively
          parseElementSequence(closingTokenPredicate, allowSemicolon, startPosForError, contextDescription, accumulatedExprs, context)
        }

      // End of file or unexpected error
      case Right(_: Token.EOF) => Left(ParseError(t"Unexpected end of file in $contextDescription", this.state.sourcePos.range.start))
      case Left(err)           => Left(err)

      // Otherwise, parse an expression
      case _ =>
        debug("Parsing expression in sequence")
        // State is already advanced past previous separator or is at the start
        parseExpr(context = context) match {
          case Left(err)   => Left(err)
          case Right(expr) =>
            // State is now after the parsed expression
            // Skip potential comments/whitespace *after* the expression
            skipComments()

            // Look ahead to see if we are followed by a separator or the closing token
            this.state.current match {
              // Found the closing token right after the expression
              case Right(token) if closingTokenPredicate(token) =>
                // Pull comments collected *after* the expression
                val trailingComments = pullComments().collect { case c: Comment => c }
                val updatedExpr = if (trailingComments.nonEmpty) {
                  expr.updateMeta { meta =>
                    mergeMeta(
                      meta,
                      createMetaWithComments(meta.flatMap(_.sourcePos), Vector.empty, trailingComments)
                    )
                  }
                } else {
                  expr
                }
                Right(accumulatedExprs :+ updatedExpr) // Return the final list

              // Found a separator after the expression
              case Right(token @ (_: Token.Comma | _: Token.Semicolon)) =>
                if (!allowSemicolon && token.isInstanceOf[Token.Semicolon]) {
                  Left(ParseError(t"Semicolon not allowed as separator in $contextDescription", token.sourcePos.range.start))
                } else {
                  debug(s"Found separator ($token) after expression")
                  // Pull comments collected *after* the expression, before the separator
                  val trailingComments = pullComments().collect { case c: Comment => c }
                  val updatedExpr = if (trailingComments.nonEmpty) {
                    expr.updateMeta { meta =>
                      mergeMeta(
                        meta,
                        createMetaWithComments(meta.flatMap(_.sourcePos), Vector.empty, trailingComments)
                      )
                    }
                  } else {
                    expr
                  }
                  // Advance past the separator
                  advance()
                  // Continue parsing elements recursively
                  parseElementSequence(
                    closingTokenPredicate,
                    allowSemicolon,
                    startPosForError,
                    contextDescription,
                    accumulatedExprs :+ updatedExpr,
                    context
                  )
                }
              // Unexpected token after expression
              case Right(other) =>
                val separatorDesc = if (allowSemicolon) "',' or ';'" else "','"
                Left(ParseError(t"Expected $separatorDesc or closing token for $contextDescription, but found $other", other.sourcePos.range.start))
              case Left(err) => Left(err)
            }
        }
    }
  }

  // Helper method to parse delimited expressions (lists, tuples, etc.)
  private def parseDelimitedExpr[T](
      context0: ReaderContext = ReaderContext(),
      openingTokenMatcher: PartialFunction[Either[ParseError, Token], Token],
      openingTokenDescription: String,
      closingTokenPredicate: Token => Boolean,
      closingTokenDescription: String,
      allowSemicolon: Boolean,
      contextDescription: String,
      createResult: (Vector[ParsedExpr], Option[ExprMeta]) => T
  ): Either[ParseError, T] = {
    val context = context0.copy(inOpSeq = false, dontallowOpSeq = false, newLineAfterBlockMeansEnds = false, dontAllowBlockApply = false)

    // Skip comments before the expression
    skipComments()

    this.state.current match {
      case Right(token) if openingTokenMatcher.isDefinedAt(Right(token)) =>
        val openToken = openingTokenMatcher(Right(token))
        val sourcePos = openToken.sourcePos

        // Advance past the opening token and skip comments
        advance()
        skipComments()

        // Parse the element sequence
        parseElementSequence(
          closingTokenPredicate = closingTokenPredicate,
          allowSemicolon = allowSemicolon,
          startPosForError = sourcePos,
          contextDescription = contextDescription,
          context = context
        ).flatMap { exprs =>
          // Skip comments after the expressions
          skipComments()

          // Check for the closing token
          this.state.current match {
            case Right(token) if closingTokenPredicate(token) =>
              // Get comments that were collected during parsing
              val comments = pullComments()
              val meta = if (comments.nonEmpty) {
                createMeta(Some(sourcePos), Some(this.state.sourcePos))
                  .map(m =>
                    ExprMeta(
                      m.sourcePos,
                      createCommentInfo(comments.collect { case c: Comment => c }, Vector.empty)
                    )
                  )
              } else {
                createMeta(Some(sourcePos), Some(this.state.sourcePos))
              }

              // Advance past the closing token
              advance()
              Right(createResult(exprs, meta))

            case _ => Left(expectedError(closingTokenDescription, this.state.current))
          }
        }
      case _ => Left(expectedError(openingTokenDescription, this.state.current))
    }
  }

  private def parseList(context0: ReaderContext = ReaderContext()): Either[ParseError, ListExpr] =
    parseDelimitedExpr(
      context0 = context0,
      openingTokenMatcher = { case Right(t: Token.LBracket) => t },
      openingTokenDescription = "[",
      closingTokenPredicate = _.isInstanceOf[Token.RBracket],
      closingTokenDescription = "']' at end of list",
      allowSemicolon = false, // Semicolons not allowed in lists
      contextDescription = "list",
      createResult = (exprs, meta) => ListExpr(exprs, meta)
    )

  private def parseTuple(context0: ReaderContext = ReaderContext()): Either[ParseError, Tuple] =
    parseDelimitedExpr(
      context0 = context0,
      openingTokenMatcher = { case Right(t: Token.LParen) => t },
      openingTokenDescription = "left parenthesis",
      closingTokenPredicate = _.isInstanceOf[Token.RParen],
      closingTokenDescription = "right parenthesis",
      allowSemicolon = false,
      contextDescription = "tuple or argument list",
      createResult = (exprs, meta) => Tuple(exprs, meta)
    )

  // Re-revised helper method to parse a sequence of statements/expressions, returning statements and optional result
  private def parseStatementSequence(
      isTerminator: ReaderState => Boolean,
      contextDescription: String,
      context: ReaderContext = ReaderContext()
  ): Either[ParseError, (Vector[ParsedExpr], Option[ParsedExpr])] = {
    var statements = Vector.empty[ParsedExpr]
    var currentResult: Option[ParsedExpr] = None

    @tailrec
    def loop(): Either[ParseError, Unit] = {
      // Skip comments/whitespace before checking for terminator or next statement
      skipComments()
      val stateBeforePossibleExpr = this.state // Remember state

      // Check for termination condition first
      if (isTerminator(this.state)) {
        debug(s"parseStatementSequence: Reached terminator for $contextDescription")
        Right(()) // Sequence finished successfully
      } else {
        // Handle explicit separators (semicolons) BEFORE parsing the expression
        var consumedSeparator = false
        while (this.state.current.exists(_.isInstanceOf[Token.Semicolon])) {
          debug(s"parseStatementSequence: Consuming explicit semicolon separator in $contextDescription")
          // If there was a result pending, it becomes a statement due to the semicolon
          currentResult.foreach { res =>
            statements = statements :+ res
            currentResult = None
          }
          advance()
          skipComments()
          consumedSeparator = true
        }

        // Check again for terminator after consuming semicolons
        if (isTerminator(this.state)) {
          // If we consumed a separator and then hit the terminator, any pending result became a statement.
          debug(s"parseStatementSequence: Reached terminator after consuming semicolon(s) for $contextDescription")
          Right(()) // Sequence finished successfully
        } else {
          // If we are here, we expect an expression.
          // If there was a previous result and we *didn't* consume an explicit semicolon,
          // we need to check for implicit separation (like a significant newline) based on tokens between.
          if (!consumedSeparator && currentResult.isDefined) {
            // Check if newline handling is enabled and if there was significant whitespace
            // Use state *before* skipping comments for whitespace check
            val whitespaceTokens = stateBeforePossibleExpr.pendingTokens.collect { case w: Token.Whitespace => w }
            val hasSignificantNewline = context.newLineAfterBlockMeansEnds &&
              whitespaceTokens.exists(isNewlineWhitespace)
            if (hasSignificantNewline) {
              debug(s"parseStatementSequence: Detected significant newline separator in $contextDescription")
              // Treat newline as separator: move result to statements
              statements = statements :+ currentResult.get
              currentResult = None
            } else {
              // Not separated by semicolon or significant newline, likely an OpSeq continuation or error
              // Let parseExpr handle it. If parseExpr succeeds, the previous result becomes a statement.
              debug(s"parseStatementSequence: No explicit/newline separator, assuming OpSeq continuation or error in $contextDescription")
              statements = statements :+ currentResult.get
              currentResult = None
            }
          } else if (currentResult.isDefined && consumedSeparator) {
            // Result already moved to statements above if separator was consumed
          }

          // --- Parse the next expression ---
          debug(s"parseStatementSequence: Attempting to parse expression in $contextDescription at ${this.state.current}")
          parseExpr(context = context) match {
            case Left(err) =>
              // Check if the error occurred because we expected an expression but found a terminator
              if (isTerminator(this.state)) {
                debug(s"parseStatementSequence: Hit terminator instead of expected expression in $contextDescription. Ending sequence.")
                Right(()) // End sequence gracefully if terminator found where expression expected
              } else {
                debug(s"parseStatementSequence: Error parsing expression in $contextDescription: $err")
                Left(err)
              }
            case Right(parsedExpr) =>
              val stateAfterExpr = this.state // State *after* parsing the expression
              debug(s"parseStatementSequence: Parsed expression $parsedExpr in $contextDescription, state after: $stateAfterExpr")

              // --- Check for '}\n' termination ---
              val endedWithBrace = stateAfterExpr.previousNonCommentToken.exists(_.isInstanceOf[Token.RBrace])
              val followedByNewlineOrEOF = {
                var tempState = stateAfterExpr
                // Look ahead past comments/whitespace
                while (
                  !tempState.isAtEnd && tempState.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])
                )
                  tempState = tempState.advance()
                // Check if the next significant token is EOF or includes a newline
                tempState.current.fold(
                  _ => false, // Error token
                  token =>
                    token.isInstanceOf[Token.EOF] || (token match {
                      case ws: Token.Whitespace => isNewlineWhitespace(ws)
                      case _                    => false // Not EOF or newline whitespace
                    })
                )
              }

              if (endedWithBrace && followedByNewlineOrEOF && context.newLineAfterBlockMeansEnds) {
                debug(s"parseStatementSequence: Expression ending with '}' followed by newline/EOF in $contextDescription. Treating as statement.")
                statements = statements :+ parsedExpr
                currentResult = None
                // Skip comments/whitespace *after* the expression to check the actual terminator
                skipComments()
                if (isTerminator(this.state)) {
                  debug(s"parseStatementSequence: Terminator found after treating '}\\n' as statement end in $contextDescription.")
                  Right(()) // End loop, sequence finished
                } else {
                  loop() // Continue parsing more statements
                }
              } else {
                // --- Original logic: Treat as potential result ---
                currentResult = Some(parsedExpr)
                // Peek ahead: Skip comments/whitespace and check the *very next* token
                val stateAfterExprPeek = stateAfterExpr // Use state right after parseExpr
                var tempState = stateAfterExprPeek
                // Look ahead past comments/whitespace
                while (
                  !tempState.isAtEnd && tempState.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace])
                )
                  tempState = tempState.advance()
                val isFollowedByTerminator = isTerminator(tempState) // Check terminator on the lookahead state

                if (isFollowedByTerminator) {
                  debug(s"parseStatementSequence: Terminator found immediately after expression in $contextDescription. Final result found.")
                  Right(()) // Expression is the final result, end the loop.
                } else {
                  // Not followed immediately by terminator. It might be followed by a separator, or another expression.
                  // Loop again. The start of the next loop will handle separators and moving the currentResult.
                  loop()
                }
              }
          }
        }
      }
    }

    // Start the loop
    loop().map(_ => (statements, currentResult))
  }

  // Parses a sequence of expressions, typically representing a top-level file or block
  // Returns a Block containing all parsed expressions.
  def parseTopLevel(context0: ReaderContext = ReaderContext()): Either[ParseError, Block] = {
    val context = context0.copy(newLineAfterBlockMeansEnds = true)
    val startPos = state.sourcePos // Capture start position
    debug(t"parseTopLevel: starting with state=${this.state}")

    // Call the helper to parse the sequence, terminating at EOF
    parseStatementSequence(
      isTerminator = s => s.isAtEnd || s.current.exists(_.isInstanceOf[Token.EOF]),
      contextDescription = "top-level",
      context = context
    ) match {
      case Right((statements, lastExpr)) => // Use the returned statements/result directly
        val endPos = state.sourcePos // Capture end position
        debug(t"parseTopLevel: Finished, statements=${statements.size}, has result=${lastExpr.isDefined}")
        // Construct the Block
        Right(Block(statements, lastExpr, createMeta(Some(startPos), Some(endPos))))
      case Left(err) =>
        debug(t"parseTopLevel: Error parsing sequence: $err")
        Left(err)
    }
  }

  private def parseBlock(context0: ReaderContext = ReaderContext()): Either[ParseError, Block] = {
    val context = context0.copy(newLineAfterBlockMeansEnds = true)
    debug(t"parseBlock: starting with state=${this.state}") // State is already modified here

    this.state.current match {
      case Right(Token.LBrace(startPos)) =>
        debug("parseBlock: Found opening brace")
        advance()
        skipComments()
        val startPosForMeta = startPos

        parseStatementSequence(
          isTerminator = _.current.exists(_.isInstanceOf[Token.RBrace]),
          contextDescription = "block",
          context = context
        ).flatMap { case (statements, result) =>
          this.state.current match {
            case Right(Token.RBrace(endPos)) =>
              debug(t"parseBlock: Found closing brace, statements=${statements.size}, has result=${result.isDefined}")
              val meta = createMeta(Some(startPosForMeta), Some(endPos))
              advance() // Consume the closing brace
              Right(Block(statements, result, meta))
            case Right(t)  => Left(ParseError("Expected '}' at end of block", t.sourcePos.range.start))
            case Left(err) => Left(err)
          }
        }

      case Right(t) =>
        debug(t"parseBlock: Expected '{' but found $t")
        Left(ParseError("Expected '{' at start of block", t.sourcePos.range.start))
      case Left(err) =>
        debug(t"parseBlock: Error at start of block: $err")
        Left(err)
    }
  }

  // Helper method to check if whitespace contains a newline
  private def isNewlineWhitespace(token: Token): Boolean =
    source.readContent.toOption.exists { src =>
      val startPos = token.sourcePos.range.start.index.utf16.asInt
      val endPos = token.sourcePos.range.end.index.utf16.asInt
      startPos < src.length && endPos <= src.length &&
      src.substring(startPos, endPos).contains('\n')
    }

  private def parseFields(clauses: Vector[ObjectClause], context: ReaderContext = ReaderContext()): Either[ParseError, Vector[ObjectClause]] =
    this.state.current match {
      case Right(Token.RBrace(_)) => Right(clauses)
      case Right(Token.Identifier(chars, idSourcePos)) =>
        val identifier = Identifier(charsToString(chars), createMeta(Some(idSourcePos), Some(idSourcePos)))
        advance()
        this.state.current match {
          case Right(Token.Dot(_)) =>
            handleDotCall(this.state.sourcePos, Vector(identifier)).flatMap { dotExpr =>
              skipComments()
              parseField(dotExpr, idSourcePos).flatMap(clause => checkAfterField().flatMap(_ => parseFields(clauses :+ clause)))
            }
          case _ =>
            skipComments()
            parseField(identifier, idSourcePos).flatMap(clause => checkAfterField().flatMap(_ => parseFields(clauses :+ clause)))
        }
      case Right(Token.StringLiteral(chars, strSourcePos)) =>
        val stringLiteral = StringLiteral(charsToString(chars), createMeta(Some(strSourcePos), Some(strSourcePos)))
        advance()
        skipComments()
        parseField(stringLiteral, strSourcePos).flatMap(clause => checkAfterField().flatMap(_ => parseFields(clauses :+ clause)))
      case Right(Token.SymbolLiteral(value, symSourcePos)) =>
        val symbolLiteral = chester.syntax.concrete.SymbolLiteral(value, createMeta(Some(symSourcePos), Some(symSourcePos)))
        advance()
        skipComments()
        parseField(symbolLiteral, symSourcePos).flatMap(clause => checkAfterField().flatMap(_ => parseFields(clauses :+ clause)))
      case Right(t) =>
        Left(ParseError("Expected identifier, string literal, symbol literal or '}' in object", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }

  private def parseField(key: ParsedExpr, keySourcePos: SourcePos, context: ReaderContext = ReaderContext()): Either[ParseError, ObjectClause] =
    this.state.current match {
      case Right(Token.Operator(op, _)) =>
        advance()
        parseExpr().flatMap { value =>
          if (op == "=>") {
            Right(ObjectExprClauseOnValue(key, value))
          } else if (op == "=") {
            key match {
              case id: Identifier =>
                Right(ObjectExprClause(id, value))
              case dotCall: DotCall =>
                Right(ObjectExprClause(dotCall, value))
              case stringLit: StringLiteral =>
                val idKey = Identifier(stringLit.value, createMeta(Some(keySourcePos), Some(keySourcePos)))
                Right(ObjectExprClause(idKey, value))
              case other =>
                Left(ParseError(t"Expected identifier, qualified name, or dot expression for key with = operator: $other", keySourcePos.range.start))
            }
          } else {
            Left(ParseError(t"Unexpected operator in object field: $op", keySourcePos.range.start))
          }
        }
      case Right(t)  => Left(ParseError("Expected operator in object field", t.sourcePos.range.start))
      case Left(err) => Left(err)
    }

  private def checkAfterField(context: ReaderContext = ReaderContext()): Either[ParseError, Unit] =
    this.state.current match {
      case Right(Token.Comma(_)) =>
        advance()
        skipComments()
        Right(())
      case Right(Token.RBrace(_)) => Right(())
      case Right(t)               => Left(ParseError("Expected ',' or '}' after object field", t.sourcePos.range.start))
      case Left(err)              => Left(err)
    }

  private def parseObject(context: ReaderContext = ReaderContext()): Either[ParseError, ObjectExpr] = {
    // Skip comments before the object
    skipComments()

    this.state.current match {
      case Right(Token.LBrace(sourcePos)) =>
        // Advance past the opening brace and skip comments
        advance()
        skipComments()

        parseFields(Vector.empty).flatMap { clauses =>
          this.state.current match {
            case Right(Token.RBrace(endPos)) =>
              // Create meta with comments that were collected during parsing
              val comments = pullComments()
              val objectMeta = if (comments.nonEmpty) {
                val meta = createMeta(Some(sourcePos), Some(endPos))
                meta.map(m => ExprMeta(m.sourcePos, createCommentInfo(comments)))
              } else {
                createMeta(Some(sourcePos), Some(endPos))
              }

              // Advance past the closing brace
              advance()
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

  private type CommOrWhite = Comment | Token.Whitespace

  /** Helper method to advance the lexer state by one token
    * @return
    *   the previous state before advancing
    */
  private inline def advance(): Unit =
    this.state = this.state.advance()

  /** Helper method to clear pending tokens from the lexer state
    * @return
    *   the previous state before clearing
    */
  private inline def clearPendingTokens(): Unit =
    this.state = this.state.clearPendingTokens()

  /** Skips all comments and whitespace tokens, updating this.state directly. All skipped tokens are automatically added to the state's pendingTokens
    * for later retrieval.
    */
  private def skipComments(): Unit =
    while (!this.state.isAtEnd && this.state.current.exists(token => token.isInstanceOf[Token.Comment] || token.isInstanceOf[Token.Whitespace]))
      advance() // This automatically adds comments/whitespace to pendingTokens

  /** Retrieves all pending tokens (comments and whitespace) and converts them to CommOrWhite format, then clears the pendingTokens collection. This
    * also has the side effect of updating this.state.
    * @return
    *   Vector of collected comments and whitespace tokens
    */
  private def pullComments(): Vector[CommOrWhite] = {
    // Get the pending tokens from the current state
    val pendingTokens = this.state.pendingTokens

    // Clear the pending tokens in the state
    clearPendingTokens()

    // Convert and return the tokens
    pendingTokens.map {
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
  }

  // Method collectComments has been removed and replaced with skipComments() and pullComments()

  /** Collects trailing comments after an expression until a newline or non-comment token. Modifies this.state directly (via skipComments).
    * @return
    *   Vector of comments collected
    */
  private def collectTrailingComments(): Vector[Comment] = {
    // Skip all comments and whitespace, storing them in pendingTokens
    skipComments()

    // Pull the comments and filter to only return Comments (not Whitespace)
    pullComments().collect { case c: Comment => c }
  }

  /** Creates ExprMeta with comments.
    */
  private def createMetaWithComments(
      sourcePos: Option[SourcePos],
      leadingComments: Vector[CommOrWhite] = Vector.empty,
      trailingComments: Vector[Comment] = Vector.empty
  ): Option[ExprMeta] =
    ExprMeta.maybe(sourcePos, createCommentInfo(leadingComments, trailingComments))

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

  // Core implementation for comment handling
  private def withCommentsCore[T <: ParsedExpr](
      parser: => Either[ParseError, T]
  ): Either[ParseError, T] = {
    // Skip comments at the start
    skipComments()

    // Get any collected comments
    val leadingComments = pullComments()

    val result = parser.flatMap { expr =>
      // Collect trailing comments (state is updated directly by the method)
      val trailingComments = collectTrailingComments()

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
    result
  }

  /** Generic parser combinator that adds comment handling to any parse method */
  private def withComments[T <: ParsedExpr](
      parseMethod: () => Either[ParseError, T]
  ): Either[ParseError, T] = withCommentsCore(parseMethod())

  // Helper that combines collecting comments and parsing with error handling
  private def withCommentsAndErrorHandling[T <: ParsedExpr](
      parser: => Either[ParseError, T],
      errorMsg: String
  ): Either[ParseError, T] = withCommentsCore(withErrorHandling(parser, errorMsg))

  // Create a helper method for parsing literals with common pattern
  private def parseLiteral[T <: ParsedExpr](
      extract: Token => Option[(String, SourcePos)],
      create: (String, Option[ExprMeta]) => T,
      errorMsg: String
  ): Either[ParseError, T] =
    this.state.current match {
      case Right(token) =>
        extract(token) match {
          case Some((value, sourcePos)) =>
            val meta = createMeta(Some(sourcePos), None)
            // Advance the state after extracting the token
            advance()
            Right(create(value, meta))
          case None =>
            Left(ParseError(errorMsg, this.state.sourcePos.range.start))
        }
      case Left(err) => Left(err)
    }

}
