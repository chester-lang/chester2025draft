package chester.reader

import chester.error.Reporter
import chester.readerv2.LexerV2
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*

def parseAndCheck(input: String, expected: Expr): Unit = {
  val resultignored = ChesterReader.parseExpr(
    FileNameAndContent("testFile", input)
  ) // it must parse with location
  ChesterReader
    .parseExpr(
      FileNameAndContent("testFile", input),
      ignoreLocation = true
    )
    .fold(
      error =>
        fail(
          s"Parsing failed for input: $input ${error.message} at index ${error.pos}"
        ),
      { value =>
        assertEquals(read[Expr](write[Expr](value)), value)
        assertEquals(
          read[Expr](write[Expr](resultignored.right.get)),
          resultignored.right.get
        )
        assertEquals(readBinary[Expr](writeBinary[Expr](value)), value)
        assertEquals(
          readBinary[Expr](writeBinary[Expr](resultignored.right.get)),
          resultignored.right.get
        )
        assertEquals(value, expected, s"Failed for input: $input")
      }
    )
}

def getParsed(input: String): Expr = {
  ChesterReader.parseExpr(
    FileNameAndContent("testFile", input)
  ) // it must parse with location
  ChesterReader
    .parseExpr(
      FileNameAndContent("testFile", input),
      ignoreLocation = true
    )
    .fold(
      error =>
        fail(
          s"Parsing failed for input: $input ${error.message} at index ${error.pos}"
        ),
      value => value
    )
}

def parseAndCheckV2(input: String, expected: Expr): Unit = {
  // Check old implementation first
  parseAndCheck(input, expected)

  // Check new implementation
  val source = FileNameAndContent("testFile", input)
  given reporter: Reporter[ParseError] = new Reporter[ParseError] {
    def apply(error: ParseError): Unit = fail(s"Tokenizer error: ${error.message}")
  }
  val sourceOffset = SourceOffset(source)
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  val tokens = tokenizer.tokenize
  val lexerState = LexerV2(tokens, sourceOffset, ignoreLocation = true)

  LexerV2
    .parseExpr(lexerState)
    .fold(
      error => fail(s"V2 Parsing failed for input: $input ${error.message} at ${error.pos}"),
      { case (value, _) =>
        assertEquals(value, expected, s"V2 implementation failed for input: $input")
        // Also verify serialization works
        assertEquals(read[Expr](write[Expr](value)), value)
        assertEquals(readBinary[Expr](writeBinary[Expr](value)), value)
      }
    )
}
