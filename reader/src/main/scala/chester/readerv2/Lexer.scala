package chester.readerv2

import chester.error.Pos
import chester.reader.ParseError
import chester.readerv2.Token.*

case class LexerState(
    tokens: TokenStream,
    current: Token,
    errors: Vector[ParseError] = Vector.empty
)

object Lexer {
  def apply(tokens: TokenStream): LexerState = {
    tokens.headOption match {
      case Some(Right(token)) => LexerState(tokens.tail, token)
      case Some(Left(error)) => LexerState(tokens.tail, EOF(error.pos), Vector(error))
      case None => LexerState(LazyList.empty, EOF(Pos.zero))
    }
  }

  def advance(state: LexerState): LexerState = {
    state.tokens.headOption match {
      case Some(Right(token)) =>
        state.copy(tokens = state.tokens.tail, current = token)
      case Some(Left(error)) =>
        state.copy(
          tokens = state.tokens.tail,
          errors = state.errors :+ error
        )
      case None => state
    }
  }

  def skipWhitespaceAndComments(state: LexerState): LexerState = {
    state.current match {
      case _: Whitespace | _: SingleLineComment => skipWhitespaceAndComments(advance(state))
      case _ => state
    }
  }

  def peek(state: LexerState): Option[Token] = {
    state.tokens.headOption match {
      case Some(Right(token)) => Some(token)
      case _ => None
    }
  }

  def expect(state: LexerState, tokenType: Class[? <: Token]): Either[ParseError, LexerState] = {
    if (tokenType.isInstance(state.current)) {
      Right(advance(state))
    } else {
      Left(ParseError(
        s"Expected ${tokenType.getSimpleName} but got ${state.current.getClass.getSimpleName}", 
        state.current.pos
      ))
    }
  }

  def matchToken(state: LexerState, f: Token => Boolean): Boolean = {
    f(state.current)
  }

  def isOperator(token: Token): Boolean = token match {
    case Operator(_,_) => true
    case _ => false
  }

  def isIdentifier(token: Token): Boolean = token match {
    case Identifier(_,_) => true
    case _ => false
  }
}

