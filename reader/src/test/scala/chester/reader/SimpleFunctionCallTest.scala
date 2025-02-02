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
} 