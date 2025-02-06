package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class SimpleFunctionCallTest extends FunSuite {
  test("simplest function call - no args") {
    val input = "f()"
    val expected = FunctionCall(
      Identifier("f", meta = None),
      Tuple(Vector.empty, meta = None),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("simple plus function") {
    val input = "+(2, 3)"
    val expected = FunctionCall(
      Identifier("+", meta = None),
      Tuple(
        Vector(
          IntegerLiteral(2, meta = None),
          IntegerLiteral(3, meta = None)
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("function call with one arg") {
    val input = "foo(1)"
    val expected = FunctionCall(
      Identifier("foo", meta = None),
      Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("function call with multiple args") {
    val input = "foo(1, 2, 3)"
    val expected = FunctionCall(
      Identifier("foo", meta = None),
      Tuple(
        Vector(
          IntegerLiteral(1, meta = None),
          IntegerLiteral(2, meta = None),
          IntegerLiteral(3, meta = None)
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("nested function calls") {
    val input = "foo(bar())"
    val expected = FunctionCall(
      Identifier("foo", meta = None),
      Tuple(
        Vector(
          FunctionCall(
            Identifier("bar", meta = None),
            Tuple(Vector.empty, meta = None),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("function call with mixed arg types") {
    val input = """foo(1, "test", bar())"""
    val expected = FunctionCall(
      Identifier("foo", meta = None),
      Tuple(
        Vector(
          IntegerLiteral(1, meta = None),
          StringLiteral("test", meta = None),
          FunctionCall(
            Identifier("bar", meta = None),
            Tuple(Vector.empty, meta = None),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }
}
