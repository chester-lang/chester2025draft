package chester.integrity

import chester.reader.FileNameAndContent
import chester.syntax.concrete.*
import chester.i18n.*
import chester.readerv1.ChesterReaderV1
import chester.error.reporterToEither

// Test that the binary is still performing well when compiled differently.
object IntegrityCheck {
  private val tests = scala.collection.mutable.ListBuffer[() => Unit]()

  private def test(name: String)(f: => Unit): Unit =
    tests += (() => {
      println(t"Running test: $name")
      f
    })

  private def assertEquals[T](
      actual: T,
      expected: T,
      message: String = ""
  ): Unit =
    if (actual != expected) {
      throw new AssertionError(
        t"$message. Expected: $expected, Actual: $actual"
      )
    }

  private def fail(message: String): Unit =
    throw new AssertionError(message)

  private def parseAndCheck(input: String, expected: Expr): Unit = {
    // it must parse with location
    val _ = reporterToEither(
      ChesterReaderV1.parseExpr(
        FileNameAndContent("testFile", input)
      )
    )
    reporterToEither(
      ChesterReaderV1
        .parseExpr(
          FileNameAndContent("testFile", input),
          ignoreLocation = true
        )
    )
      .fold(
        error =>
          fail(
            t"Parsing failed for input: $input ${error.message} at index ${error.span0}"
          ),
        value => assertEquals(value, expected, t"Failed for input: $input")
      )
  }

  test("parse opSeq with mixed operators without spaces") {
    val input = "1+2*4+5"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None),
        Identifier("*", meta = None),
        IntegerLiteral(4, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(5, meta = None)
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse Identifier to IntegerLiteral") {
    val input = "Identifier(\"b\") -> IntegerLiteral(2)"
    val expected = OpSeq(
      Vector(
        FunctionCall(
          Identifier("Identifier", meta = None),
          Tuple(Vector(StringLiteral("b", meta = None)), meta = None),
          meta = None
        ),
        Identifier("->", meta = None),
        FunctionCall(
          Identifier("IntegerLiteral", meta = None),
          Tuple(Vector(IntegerLiteral(2, meta = None)), meta = None),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse OpSeq with identifier to integer literal mapping") {
    val input = "Vector(\n  Identifier(\"b\") -> IntegerLiteral(2)\n)"
    val expected = FunctionCall(
      Identifier("Vector", meta = None),
      Tuple(
        Vector(
          OpSeq(
            Vector(
              FunctionCall(
                Identifier("Identifier", meta = None),
                Tuple(Vector(StringLiteral("b", meta = None)), meta = None),
                meta = None
              ),
              Identifier("->", meta = None),
              FunctionCall(
                Identifier("IntegerLiteral", meta = None),
                Tuple(Vector(IntegerLiteral(2, meta = None)), meta = None),
                meta = None
              )
            ),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  def apply(): Unit =
    tests.foreach(_())
}
