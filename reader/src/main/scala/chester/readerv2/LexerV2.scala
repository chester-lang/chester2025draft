package chester.readerv2
import chester.error.{Pos, RangeInFile, Reporter, SourcePos}
import chester.reader.{ParseError, SourceOffset}
import chester.syntax.concrete.*
import chester.syntax.Name

case class LexerState(
    tokens: TokenStream,
    current: Token,
    ignoreLocation: Boolean = false,
    sourceOffset: SourceOffset
)

class LexerV2(using reporter: Reporter[ParseError]) {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false): LexerState = {
    tokens.headOption match {
      case Some(Right(token)) => 
        LexerState(tokens.tail, token, ignoreLocation = ignoreLocation, sourceOffset = sourceOffset)
      case Some(Left(error)) =>
        reporter.report(error)
        // Recover by creating a state with EOF token
        LexerState(LazyList.empty, Token.EOF(error.pos), ignoreLocation = ignoreLocation, sourceOffset = sourceOffset)
      case None => 
        LexerState(LazyList.empty, Token.EOF(Pos.zero), ignoreLocation = ignoreLocation, sourceOffset = sourceOffset)
    }
  }

  private def createMeta(pos: Pos, sourceOffset: SourceOffset): ExprMeta = {
    ExprMeta(Some(SourcePos(sourceOffset, RangeInFile(pos, pos))), None)
  }

  def parseAtom(state: LexerState): (Expr, LexerState) = {
    def parseExpr(state: LexerState): (Expr, LexerState) = {
      val cleanState = skipWhitespaceAndComments(state)
      cleanState.current match {
        case Token.IntegerLiteral(value, radix, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (IntegerLiteral(value, meta), advance(cleanState))

        case Token.RationalLiteral(value, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (RationalLiteral(value.toDouble, meta), advance(cleanState))

        case Token.StringLiteral(parts, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (
            StringLiteral(
              parts.map {
                case Token.StringChars(chars) => chars.mkString
                case Token.StringEscape(c) => c.toString
                case Token.StringInterpolation(expr) => "${" + expr.map(_.text).mkString + "}"
              }.mkString,
              meta
            ),
            advance(cleanState)
          )

        case Token.LBrace(pos) =>
          val (statements, lastExpr, state) = parseStatements(advance(cleanState))
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          state.current match {
            case Token.RBrace(_) =>
              (Block(statements, lastExpr, meta), advance(state))
            case _ =>
              reporter.report(ParseError("Expected closing brace", state.current.pos))
              // Recover by treating as end of block
              (Block(statements, lastExpr, meta), state)
          }

        case Token.LParen(pos) =>
          val (exprs, state) = parseExprList(advance(cleanState))
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          state.current match {
            case Token.RParen(_) =>
              (Tuple(exprs, meta), advance(state))
            case _ =>
              reporter.report(ParseError("Expected closing parenthesis", state.current.pos))
              // Recover by treating as end of tuple
              (Tuple(exprs, meta), state)
          }

        case Token.Operator(symbol, pos) =>
          val (expr, state) = parseAtom(advance(cleanState))
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (OpSeq(Vector(Identifier(Name(symbol), meta), expr), None), state)

        case Token.SymbolLiteral(name, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (SymbolLiteral(Name(name), meta), advance(cleanState))

        case Token.Keyword(name, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          val telescope = Vector.empty[MaybeTelescope]
          (Keyword(Name(name), telescope, meta), advance(cleanState))

        case Token.Identifier(parts, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (Identifier(Name(parts.map(_.text).mkString), meta), advance(cleanState))

        case Token.EOF(pos) =>
          reporter.report(ParseError("Unexpected end of input", pos))
          // Recover with empty tuple
          (Tuple(Vector.empty, None), state)

        case _ =>
          reporter.report(ParseError(s"Unexpected token: ${cleanState.current}", cleanState.current.pos))
          // Recover with empty tuple and advance
          (Tuple(Vector.empty, None), advance(cleanState))
      }
    }

    // Parse first atom
    val (firstExpr, firstState) = parseExpr(state)
    var current = skipWhitespaceAndComments(firstState)
    var expr = firstExpr
    var terms = Vector(expr)

    // Look for operator sequences
    while (true) {
      current.current match {
        case op @ Token.Operator(_, _) =>
          val opText = op.text
          current = skipWhitespaceAndComments(advance(current))
          val (nextExpr, nextState) = parseAtom(current)
          terms = terms :+ Identifier(Name(opText), if (current.ignoreLocation) None else Some(createMeta(op.pos, current.sourceOffset))) :+ nextExpr
          current = skipWhitespaceAndComments(nextState)
        case id @ Token.Identifier(parts, _) if parts.exists(_.isInstanceOf[Token.OpPart]) =>
          val opText = id.text
          current = skipWhitespaceAndComments(advance(current))
          val (nextExpr, nextState) = parseAtom(current)
          terms = terms :+ Identifier(Name(opText), if (current.ignoreLocation) None else Some(createMeta(id.pos, current.sourceOffset))) :+ nextExpr
          current = skipWhitespaceAndComments(nextState)
        case _ =>
          if (terms.length > 1) {
            return (OpSeq(terms, None), current)
          } else {
            return (expr, current)
          }
      }
    }
    (expr, current)
  }

  private def parseObjectExprClause(state: LexerState): (ObjectExprClause, LexerState) = {
    val (nameToken, state1) = expect(state, classOf[Token.Identifier])
    val state2 = skipWhitespaceAndComments(advance(state1))
    val meta = if (state.ignoreLocation) None else Some(createMeta(nameToken.pos, state.sourceOffset))
    
    state2.current match {
      case Token.Operator("=", _) =>
        val state3 = skipWhitespaceAndComments(advance(state2))
        val (value, state4) = parseOpSeq(state3)
        (ObjectExprClause(Identifier(Name(nameToken.text), meta), value), state4)
      case _ =>
        reporter.report(ParseError("Expected equals", state2.current.pos))
        // Recover by treating as empty value
        (ObjectExprClause(Identifier(Name(nameToken.text), meta), Tuple(Vector.empty, None)), state2)
    }
  }

  def advance(state: LexerState): LexerState = {
    var currentState = state
    while (true) {
      currentState.tokens.headOption match {
        case Some(Right(token)) =>
          return currentState.copy(current = token, tokens = currentState.tokens.tail)
        case Some(Left(error)) =>
          reporter.report(error)
          currentState = currentState.copy(tokens = currentState.tokens.tail)
        case None =>
          return currentState.copy(current = Token.EOF(currentState.current.pos), tokens = LazyList.empty)
      }
    }
    currentState // Unreachable but needed for compilation
  }

  def skipWhitespaceAndComments(state: LexerState): LexerState = {
    var currentState = state
    while (
      currentState.current match {
        case _: Token.Whitespace | _: Token.SingleLineComment => true
        case _                                                => false
      }
    ) {
      currentState = advance(currentState)
    }
    currentState
  }

  def peek(state: LexerState): Option[Token] = {
    state.tokens.headOption match {
      case Some(Right(token)) => Some(token)
      case _                  => None
    }
  }

  def parseExpr(state: LexerState): (Expr, LexerState) = {
    parseOpSeq(state)
  }

  private def parseOpSeq(state: LexerState): (Expr, LexerState) = {
    val (firstExpr, firstState) = parseAtom(state)
    var current = skipWhitespaceAndComments(firstState)
    var expr = firstExpr
    var terms = Vector(expr)

    // Look for operator sequences
    while (true) {
      current.current match {
        case op @ Token.Operator(_, _) =>
          val opText = op.text
          current = skipWhitespaceAndComments(advance(current))
          val (nextExpr, nextState) = parseAtom(current)
          terms = terms :+ Identifier(Name(opText), if (current.ignoreLocation) None else Some(createMeta(op.pos, current.sourceOffset))) :+ nextExpr
          current = skipWhitespaceAndComments(nextState)
        case id @ Token.Identifier(parts, _) if parts.exists(_.isInstanceOf[Token.OpPart]) =>
          val opText = id.text
          current = skipWhitespaceAndComments(advance(current))
          val (nextExpr, nextState) = parseAtom(current)
          terms = terms :+ Identifier(Name(opText), if (current.ignoreLocation) None else Some(createMeta(id.pos, current.sourceOffset))) :+ nextExpr
          current = skipWhitespaceAndComments(nextState)
        case _ =>
          if (terms.length > 1) {
            return (OpSeq(terms, None), current)
          } else {
            return (expr, current)
          }
      }
    }
    (expr, current)
  }

  private def parseStatements(state: LexerState): (Vector[Expr], Option[Expr], LexerState) = {
    var statements = Vector.empty[Expr]
    var current = skipWhitespaceAndComments(state)
    var lastExpr: Option[Expr] = None

    while (current.current != Token.EOF(current.current.pos) && current.current != Token.RBrace(current.current.pos)) {
      val (expr, nextState) = parseOpSeq(current)
      current = skipWhitespaceAndComments(nextState)
      current.current match {
        case Token.Comma(_) | Token.Semicolon(_) =>
          statements = statements :+ expr
          current = skipWhitespaceAndComments(advance(current))
        case _ =>
          lastExpr = Some(expr)
          current = skipWhitespaceAndComments(current)
      }
    }
    (statements, lastExpr, current)
  }

  private def parseExprList(state: LexerState): (Vector[Expr], LexerState) = {
    var exprs = Vector.empty[Expr]
    var current = skipWhitespaceAndComments(state)

    while (current.current != Token.EOF(current.current.pos) && 
           !current.current.isInstanceOf[Token.RParen] && 
           !current.current.isInstanceOf[Token.RBrace] && 
           !current.current.isInstanceOf[Token.RBracket]) {
      val (expr, nextState) = parseOpSeq(current)
      exprs = exprs :+ expr
      current = skipWhitespaceAndComments(nextState)
      current.current match {
        case Token.Comma(_) =>
          current = skipWhitespaceAndComments(advance(current))
          // Handle trailing comma
          if (current.current.isInstanceOf[Token.RParen] || 
              current.current.isInstanceOf[Token.RBrace] || 
              current.current.isInstanceOf[Token.RBracket]) {
            return (exprs, current)
          }
        case Token.RParen(_) | Token.RBrace(_) | Token.RBracket(_) =>
          // End of list
          return (exprs, current)
        case _ if exprs.nonEmpty =>
          reporter.report(ParseError("Expected comma between expressions", current.current.pos))
          // Recover by treating as end of list
          return (exprs, current)
        case _ =>
          // Single expression or empty list
      }
    }
    (exprs, current)
  }

  private def parseObjectFields(state: LexerState): (Vector[ObjectClause], LexerState) = {
    def loop(state: LexerState, acc: Vector[ObjectClause]): (Vector[ObjectClause], LexerState) = {
      val cleanState = skipWhitespaceAndComments(state)
      cleanState.current match {
        case _: Token.RBrace => (acc, cleanState)
        case _: Token.Comma  => loop(advance(cleanState), acc)
        case _ =>
          val (field, state1) = parseObjectField(cleanState)
          loop(state1, acc :+ field)
      }
    }
    loop(state, Vector.empty)
  }

  private def parseObjectField(state: LexerState): (ObjectClause, LexerState) = {
    val (nameToken, state1) = expect(state, classOf[Token.Identifier])
    val (_, state2) = expect(state1, classOf[Token.Equal])
    val (value, state3) = parseOpSeq(state2)
    (
      ObjectExprClause(
        Identifier(Name(nameToken.text), if (state.ignoreLocation) None else Some(createMeta(nameToken.pos, state.sourceOffset))),
        value
      ),
      state3
    )
  }

  private def expect[T <: Token](state: LexerState, tokenType: Class[T]): (T, LexerState) = {
    if (tokenType.isInstance(state.current)) {
      val token = state.current.asInstanceOf[T]
      val nextState = advance(state)
      (token, nextState)
    } else {
      reporter.report(ParseError(s"Expected ${tokenType.getSimpleName} but got ${state.current.getClass.getSimpleName}", state.current.pos))
      // Recover by treating as end of input
      (state.current.asInstanceOf[T], state)
    }
  }
}
