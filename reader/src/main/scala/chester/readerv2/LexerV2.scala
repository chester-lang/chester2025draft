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

object LexerV2 {
  def apply(tokens: TokenStream, sourceOffset: SourceOffset, ignoreLocation: Boolean = false)(using reporter: Reporter[ParseError]): LexerState = {
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

  def parseExpr(state: LexerState)(using reporter: Reporter[ParseError]): Either[ParseError, (Expr, LexerState)] = {
    parseAtom(state) match {
      case (expr, state) => Right((expr, state))
    }
  }

  private def parseAtom(state: LexerState)(using reporter: Reporter[ParseError]): (Expr, LexerState) = {
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
          (OpSeq(Vector(Identifier(symbol, meta), expr), None), state)

        case Token.SymbolLiteral(name, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          (SymbolLiteral(name, meta), advance(cleanState))

        case Token.Keyword(name, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          val telescope = Vector.empty[MaybeTelescope]
          (Keyword(Name(name), telescope, meta), advance(cleanState))

        case Token.Identifier(parts, pos) =>
          val meta = if (cleanState.ignoreLocation) None else Some(createMeta(pos, cleanState.sourceOffset))
          val ident = Identifier(parts.map(_.text).mkString, meta)
          var current = advance(cleanState)
          var expr: Expr = ident

          // Parse function calls and dot access
          while (current.current != Token.EOF(current.current.pos)) {
            current = skipWhitespaceAndComments(current)
            current.current match {
              case Token.LParen(_) =>
                val (args, newState) = parseExprList(advance(current))
                newState.current match {
                  case Token.RParen(_) =>
                    expr = FunctionCall(expr, Tuple(args, None), meta)
                    current = advance(newState)
                  case _ =>
                    reporter.report(ParseError("Expected closing parenthesis", newState.current.pos))
                    // Recover and continue
                    expr = FunctionCall(expr, Tuple(args, None), meta)
                    current = newState
                }

              case Token.Dot(_) =>
                current = skipWhitespaceAndComments(advance(current))
                current.current match {
                  case Token.Identifier(parts, _) =>
                    expr = DotCall(expr, Identifier(parts.map(_.text).mkString, None), Vector.empty, meta)
                    current = advance(current)
                  case _ =>
                    reporter.report(ParseError("Expected identifier after dot", current.current.pos))
                    return (expr, current)
                }

              case _ =>
                return (expr, current)
            }
          }
          (expr, current)

        case Token.EOF(pos) =>
          reporter.report(ParseError("Unexpected end of input", pos))
          (Block(Vector.empty, None, None), state)

        case token =>
          reporter.report(ParseError(s"Unexpected token: ${token.text}", token.pos))
          val meta = if (state.ignoreLocation) None else Some(createMeta(token.pos, state.sourceOffset))
          (Block(Vector.empty, None, meta), advance(state))
      }
    }

    // Parse first atom
    val (firstExpr, firstState) = parseExpr(state)
    var current = skipWhitespaceAndComments(firstState)
    var expr = firstExpr

    // Parse operator sequences
    while (current.current != Token.EOF(current.current.pos)) {
      current.current match {
        case Token.Operator(op, _) =>
          val (rightExpr, newState) = parseExpr(advance(current))
          expr = OpSeq(Vector(expr, Identifier(op, None), rightExpr), None)
          current = newState
        case _ =>
          return (expr, current)
      }
    }

    (expr, current)
  }

  private def skipWhitespaceAndComments(state: LexerState)(using reporter: Reporter[ParseError]): LexerState = {
    var current = state
    while (current.current match {
      case Token.Whitespace(_, _) | Token.SingleLineComment(_, _) => true
      case _ => false
    }) {
      current = advance(current)
    }
    current
  }

  private def advance(state: LexerState)(using reporter: Reporter[ParseError]): LexerState = {
    state.tokens.headOption match {
      case Some(Right(token)) => state.copy(tokens = state.tokens.tail, current = token)
      case Some(Left(error)) =>
        reporter.report(error)
        // Skip the error token and continue
        state.copy(tokens = state.tokens.tail)
      case None => state.copy(tokens = LazyList.empty, current = Token.EOF(state.current.pos))
    }
  }

  private def parseExprList(state: LexerState)(using reporter: Reporter[ParseError]): (Vector[Expr], LexerState) = {
    var exprs = Vector.empty[Expr]
    var current = skipWhitespaceAndComments(state)

    while (current.current != Token.EOF(current.current.pos) && 
           current.current != Token.RParen(current.current.pos)) {
      val (expr, newState) = parseAtom(current)
      exprs = exprs :+ expr
      current = skipWhitespaceAndComments(newState)

      current.current match {
        case Token.Comma(_) =>
          current = skipWhitespaceAndComments(advance(current))
        case Token.RParen(_) | Token.EOF(_) =>
          // End of list
        case _ =>
          reporter.report(ParseError("Expected comma or closing parenthesis", current.current.pos))
          // Try to recover by assuming this is the end of the list
          return (exprs, current)
      }
    }

    (exprs, current)
  }

  private def parseStatements(state: LexerState)(using reporter: Reporter[ParseError]): (Vector[Expr], Option[Expr], LexerState) = {
    var statements = Vector.empty[Expr]
    var current = skipWhitespaceAndComments(state)
    var lastExpr: Option[Expr] = None

    while (current.current != Token.EOF(current.current.pos) && 
           current.current != Token.RBrace(current.current.pos)) {
      val (expr, newState) = parseAtom(current)
      current = skipWhitespaceAndComments(newState)

      current.current match {
        case Token.Semicolon(_) =>
          statements = statements :+ expr
          current = skipWhitespaceAndComments(advance(current))
        case Token.RBrace(_) | Token.EOF(_) =>
          lastExpr = Some(expr)
        case _ =>
          statements = statements :+ expr
          current = skipWhitespaceAndComments(current)
      }
    }

    (statements, lastExpr, current)
  }
}
