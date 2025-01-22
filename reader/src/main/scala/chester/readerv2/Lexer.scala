package chester.readerv2

import chester.error.Pos
import chester.reader.ParseError
import chester.readerv2.Token.*

case class LexerState(
    tokens: TokenStream,
    current: Token,
    errors: Vector[ParseError] = Vector.empty
)

class Lexer(tokens: TokenStream) {
  def initialize: LexerState = {
    tokens.headOption match {
      case Some(Right(token)) => LexerState(tokens.tail, token)
      case Some(Left(error))  => LexerState(tokens.tail, EOF(error.pos), Vector(error))
      case None               => LexerState(LazyList.empty, EOF(Pos.zero))
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
      case _                          => state
    }
  }
}
