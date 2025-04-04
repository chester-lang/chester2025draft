package chester.reader
import chester.readerv2.{LexerState, LexerV2}
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*

// Only runs against V1 (original reader)
def parseAndCheckV1(input: String, expected: Expr): Unit = {
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

// Only runs against V2 parser
def parseAndCheckV2[T <: Expr](input: String, expected: T): Unit = {
  val oldDebug = LexerV2.DEBUG
  LexerV2.DEBUG = true
  
  try {
    val source = FileNameAndContent("testFile", input)
    val sourceOffset = SourceOffset(source)
    val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
    val tokens = tokenizer.tokenize().toVector
    val lexer = LexerV2(sourceOffset, ignoreLocation = true)
    val result = lexer.parseExpr(LexerState(tokens, 0))
    
    result.fold(
      error => 
        fail(
          s"""V2 Parsing failed for input: $input
             |Error: ${error.message}
             |At position ${error.pos}""".stripMargin
        ),
      { case (expr, _) => 
          println(s"V2 AST: $expr")
          assertEquals(expr, expected, s"Failed for input: $input")
      }
    )
  } finally {
    LexerV2.DEBUG = oldDebug
  }
}

// Runs against both V1 and V2 parsers
def parseAndCheckBoth(input: String, expected: Expr): Unit = {
  parseAndCheckV1(input, expected)
  parseAndCheckV2(input, expected)
}

// New function to parse with V1 parser only and return the result
def parseV1(input: String): Expr =
  ChesterReader
    .parseExpr(FileNameAndContent("testFile", input), ignoreLocation = true)
    .fold(
      error => fail(s"V1 parsing failed for input: $input ${error.message} at index ${error.pos}"),
      value => value
    )

// New function to parse with V2 parser only and return the result
def parseV2(input: String): Expr = {
  val source = FileNameAndContent("testFile", input)
  val sourceOffset = SourceOffset(source)
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  val tokens = tokenizer.tokenize()
  val lexer = LexerV2(sourceOffset, ignoreLocation = true)

  lexer
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

        fail(s"""V2 parsing failed for input: $input
             |Error: ${error.message}
             |At position ${error.pos}:
             |$line
             |$pointer""".stripMargin)
      },
      { case (expr, _) => expr }
    )
}
