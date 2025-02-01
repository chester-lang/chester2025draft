package chester.readerv2

import chester.error.Reporter
import chester.reader.{FileNameAndContent, ParseError, SourceOffset}
import chester.syntax.concrete.Expr
import io.github.iltotore.iron.*
import chester.error.Pos
import chester.utils.WithUTF16

object ChesterReaderV2 {
  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    given reporter: Reporter[ParseError] = new Reporter[ParseError] {
      def apply(error: ParseError): Unit = {}
      def const(e: ParseError): Reporter[ParseError] = this
    }
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(tokens, sourceOffset, reporter, false)
    lexer.parseExpr().map(_._1)
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    given reporter: Reporter[ParseError] = new Reporter[ParseError] {
      def apply(error: ParseError): Unit = {}
      def const(e: ParseError): Reporter[ParseError] = this
    }
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(tokens, sourceOffset, reporter, false)
    val pos = Pos(WithUTF16.Zero, 0, WithUTF16.Zero)
    lexer.parseExprList(LexerState(tokens, Right(Token.EOF(pos)), ignoreLocation = false, sourceOffset)).map(_._1)
  }
}
