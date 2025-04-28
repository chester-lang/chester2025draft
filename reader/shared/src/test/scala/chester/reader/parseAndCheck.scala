package chester.reader
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*
import chester.i18n.*
import chester.readerv1.ChesterReader
import chester.readerv2.ChesterReaderV2
import chester.utils.asInt

// Only runs against V1 (original reader)
def parseAndCheckV1(input: String, expected: Expr): Unit = {
  val resultignored = ChesterReader.parseExpr(
    FileNameAndContent("testFile", input)
  ) // it must parse with location
  val value = parseV1(input)

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
  assertEquals(value, expected, t"Failed for input: $input")
}

// Only runs against V2 parser
def parseAndCheckV2(input: String, expected: Expr): Unit = {
  val result = parseV2(input)
  assertEquals(result, expected, t"Failed for input: $input")
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
      error => fail(t"V1 parsing failed for input: $input ${error.message} at index ${error.pos}"),
      value => value
    )

// New function to parse with V2 parser only and return the result
def parseV2(input: String): Expr = {
  val source = FileNameAndContent("testFile", input)

  ChesterReaderV2
    .parseExpr(source, ignoreLocation = true)
    .fold(
      error => {
        val errorIndex = error.pos.index.utf16.asInt
        val lineStart = input.lastIndexOf('\n', errorIndex) + 1
        val lineEnd = input.indexOf('\n', errorIndex) match {
          case -1 => input.length
          case n  => n
        }
        val line = input.substring(lineStart, lineEnd)
        val pointer = " " * (errorIndex - lineStart) + "^"

        fail(t"""V2 parsing failed for input: $input
             |Error: ${error.message}
             |At position ${error.pos}:
             |$line
             |$pointer""".stripMargin)
      },
      expr => expr.descentRecursive(_.updateMeta(_ => None))
    )
}

// New function to parse with V1 parser only and return the result (using TopLevel)
def parseTopLevelV1(input: String): Expr =
  ChesterReader
    .parseTopLevel(FileNameAndContent("testFile", input), ignoreLocation = true)
    .fold(
      error => fail(t"V1 parsing failed for input: $input ${error.message} at index ${error.pos}"),
      value => value
    )

// New function to parse with V2 parser only and return the result (using TopLevel)
def parseTopLevelV2(input: String): Expr = {
  val source = FileNameAndContent("testFile", input)

  ChesterReaderV2
    .parseTopLevel(source, ignoreLocation = true)
    .fold(
      error => {
        val errorIndex = error.pos.index.utf16.asInt
        val lineStart = input.lastIndexOf('\n', errorIndex) + 1
        val lineEnd = input.indexOf('\n', errorIndex) match {
          case -1 => input.length
          case n  => n
        }
        val line = input.substring(lineStart, lineEnd)
        val pointer = " " * (errorIndex - lineStart) + "^"

        fail(t"""V2 parsing failed for input: $input
             |Error: ${error.message}
             |At position ${error.pos}:
             |$line
             |$pointer""".stripMargin)
      },
      expr => expr.descentRecursive(_.updateMeta(_ => None))
    )
}

// Runs against both V1 and V2 parsers (using TopLevel)
def parseAndCheckTopLevelBoth(input: String, expected: Expr): Unit = {
  parseAndCheckTopLevelV1(input, expected)
  parseAndCheckTopLevelV2(input, expected)
}

// Only runs against V1 parser (using TopLevel)
def parseAndCheckTopLevelV1(input: String, expected: Expr): Unit = {
  val result = parseTopLevelV1(input)
  assertEquals(result, expected, t"Failed for input: $input")
}

// Only runs against V2 parser (using TopLevel)
def parseAndCheckTopLevelV2(input: String, expected: Expr): Unit = {
  val result = parseTopLevelV2(input)
  assertEquals(result, expected, t"Failed for input: $input")
}
