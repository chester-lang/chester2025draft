package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class FunctionCallParserTest extends FunSuite {

  test("parse simple function call with no arguments") {
    val input = "func()"
    val expected = FunctionCall(
      Identifier("func", meta = None),
      Tuple(Vector(), meta = None),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse function call with multiple arguments") {
    val input = "multiply(2, 3)"
    val expected = FunctionCall(
      Identifier("multiply", meta = None),
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

  test("parse function call with multiple arguments and symbol identifier") {
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

  test("parse function call with mixed type arguments") {
    val input = "createPerson(\"John\", 30, true)"
    val expected = FunctionCall(
      Identifier("createPerson", meta = None),
      Tuple(
        Vector(
          StringLiteral("John", meta = None),
          IntegerLiteral(30, meta = None),
          Identifier("true", meta = None)
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse function call with generics and mixed type arguments") {
    val input = "createPerson[Integer](\"John\", 30, true)"
    val expected = FunctionCall(
      FunctionCall(
        Identifier("createPerson", meta = None),
        ListExpr(
          Vector(Identifier("Integer", meta = None)),
          meta = None
        ),
        meta = None
      ),
      Tuple(
        Vector(
          StringLiteral("John", meta = None),
          IntegerLiteral(30, meta = None),
          Identifier("true", meta = None)
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("simple function call") {
    val input = "func()"
    val expected = FunctionCall(
      Identifier("func", None),
      Tuple(Vector.empty, None),
      None
    )
    parseAndCheck(input, expected)
  }
}
