package chester.reader

import chester.error.Reporter
import chester.readerv2.{LexerV2, LexerState}
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*

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
@deprecated("TODO: remove this")
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

def parseAndCheck(input: String, expected: Expr): Unit = {
  // Check old implementation first
  parseAndCheckV0(input, expected)

  // Check new implementation
  val source = FileNameAndContent("testFile", input)
  given reporter: Reporter[ParseError] = new Reporter[ParseError] {
    def apply(error: ParseError): Unit = {
      val errorIndex = error.pos.index.utf16
      val lineStart = input.lastIndexOf('\n', errorIndex) + 1
      val lineEnd = input.indexOf('\n', errorIndex) match {
        case -1 => input.length
        case n  => n
      }
      val line = input.substring(lineStart, lineEnd)
      val pointer = " " * (errorIndex - lineStart) + "^"
      
      fail(
        s"""Tokenizer error: ${error.message}
           |At position ${error.pos}:
           |$line
           |$pointer""".stripMargin)
    }
  }

  val sourceOffset = SourceOffset(source)
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  val tokens = tokenizer.tokenize()
  val lexer = LexerV2(tokens, sourceOffset, ignoreLocation = true)

  val result = lexer
    .parseExpr(LexerState(tokens.toVector, 0))
    .fold(
      error => {
        val errorIndex = error.pos.index.utf16
        val lineStart = input.lastIndexOf('\n', errorIndex) + 1
        val lineEnd = input.indexOf('\n', errorIndex) match {
          case -1 => input.length
          case n  => n
        }
        val line = input.substring(lineStart, lineEnd)
        val pointer = " " * (errorIndex - lineStart) + "^"
        
        fail(
          s"""V2 Parsing failed for input: $input
             |Error: ${error.message}
             |At position ${error.pos}:
             |$line
             |$pointer""".stripMargin)
      },
      { case (expr, _) => expr }
    )

  assertEquals(result, expected, s"Failed for input: $input")
}

@deprecated("TODO: remove this")
def getParsedV1(input: String): Expr = {
  // Parse with location first to ensure it works
  ChesterReader.parseExpr(FileNameAndContent("testFile", input)).fold(
    error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
    _ => ()
  )
  
  // Then parse without location for the actual result
  ChesterReader.parseExpr(FileNameAndContent("testFile", input), ignoreLocation = true).fold(
    error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
    value => value
  )
}
