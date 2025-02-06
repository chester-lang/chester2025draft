package chester.readerv2

import chester.error.Reporter
import chester.reader.{FileNameAndContent, ParseError, SourceOffset}
import chester.syntax.concrete.Expr
import chester.error.{Pos, RangeInFile, SourcePos}

object ChesterReaderV2 {
  def parseExpr(source: FileNameAndContent): Either[ParseError, Expr] = {
    given reporter: Reporter[ParseError] = new Reporter[ParseError] {
      def apply(error: ParseError): Unit = {}
      def const(e: ParseError): Reporter[ParseError] = this
    }
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(tokens, sourceOffset, false)
    lexer.parseExpr(LexerState(tokens.toVector, 0)).map(_._1)
  }

  def parseExprList(source: FileNameAndContent): Either[ParseError, Vector[Expr]] = {
    given reporter: Reporter[ParseError] = new Reporter[ParseError] {
      def apply(error: ParseError): Unit = {}
      def const(e: ParseError): Reporter[ParseError] = this
    }
    val sourceOffset = SourceOffset(source)
    val tokenizer = new Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize()
    val lexer = new LexerV2(tokens, sourceOffset, false)
    SourcePos(sourceOffset, RangeInFile(Pos.zero, Pos.zero))
    lexer.parseExprList(LexerState(tokens.toVector, 0)).map(_._1)
  }
}
