package chester.reader

import chester.error.Reporter
import chester.readerv2.{LexerState, LexerV2}
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*

// Only runs against V1 (original reader)
def parseAndCheck(input: String, expected: Expr): Unit = parseAndCheckV0(input, expected)

// Only runs against V1 (original reader)
def parseAndCheckV0(input: String, expected: Expr): Unit = {
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
          read[Expr](write[Expr](resultignored.toOption.get)),
          resultignored.toOption.get
        )
        assertEquals(readBinary[Expr](writeBinary[Expr](value)), value)
        assertEquals(
          readBinary[Expr](writeBinary[Expr](resultignored.toOption.get)),
          resultignored.toOption.get
        )
        assertEquals(value, expected, s"Failed for input: $input")
      }
    )
}

@deprecated("Use parseAndCheckBoth instead")
def parseAndCheckV1(input: String, expected: Expr): Unit = parseAndCheckBoth(input, expected)

// Runs against both V1 and V2 parsers
def parseAndCheckBoth(input: String, expected: Expr): Unit = {
  // Check old implementation first
  parseAndCheckV0(input, expected)

  // Check new implementation
  val source = FileNameAndContent("testFile", input)
  given reporter: Reporter[ParseError] = new Reporter[ParseError] {
    def apply(error: ParseError): Unit = {
      val errorIndex = error.pos.index.utf16
      val lineStart = math.max(0, input.lastIndexOf('\n', errorIndex) + 1)
      val lineEnd = input.indexOf('\n', errorIndex) match {
        case -1 => input.length
        case n  => n
      }
      // Ensure lineEnd > lineStart to prevent StringIndexOutOfBoundsException
      val safeLineEnd = math.max(lineStart, lineEnd)
      val line = input.substring(lineStart, safeLineEnd)
      val pointer = " " * (errorIndex - lineStart) + "^"

      fail(s"""Tokenizer error: ${error.message}
           |At position ${error.pos}:
           |$line
           |$pointer""".stripMargin)
    }
  }

  val sourceOffset = SourceOffset(source)
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  val tokens = tokenizer.tokenize()
  val oldDebug = LexerV2.DEBUG
  try {
    // LexerV2.DEBUG = true // uncomment when needed
    val lexer = LexerV2(tokens, sourceOffset, ignoreLocation = true)

    val result = lexer
      .parseExpr(LexerState(tokens.toVector, 0))
      .fold(
        error => {
          val errorIndex = error.pos.index.utf16
          val lineStart = math.max(0, input.lastIndexOf('\n', errorIndex) + 1)
          val lineEnd = input.indexOf('\n', errorIndex) match {
            case -1 => input.length
            case n  => n
          }
          // Ensure lineEnd > lineStart to prevent StringIndexOutOfBoundsException
          val safeLineEnd = math.max(lineStart, lineEnd)
          val line = input.substring(lineStart, safeLineEnd)
          val pointer = " " * (errorIndex - lineStart) + "^"

          fail(s"""V2 Parsing failed for input: $input
               |Error: ${error.message}
               |At position ${error.pos}:
               |$line
               |$pointer""".stripMargin)
        },
        { case (expr, _) => expr }
      )

    assertEquals(result, expected, s"Failed for input: $input")
  } finally {
    LexerV2.DEBUG = oldDebug
  }
}

@deprecated("Use getParsedBoth instead")
def getParsed(input: String): Expr = getParsedV1(input)

@deprecated("Use getParsedBoth instead")
def getParsedV1(input: String): Expr = {
  // Parse with location first to ensure it works
  ChesterReader
    .parseExpr(FileNameAndContent("testFile", input))
    .fold(
      error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
      _ => ()
    )

  // Then parse without location for the actual result
  ChesterReader
    .parseExpr(FileNameAndContent("testFile", input), ignoreLocation = true)
    .fold(
      error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
      value => value
    )
}
