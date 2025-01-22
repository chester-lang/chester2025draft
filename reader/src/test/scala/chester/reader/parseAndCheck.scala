package chester.reader

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
