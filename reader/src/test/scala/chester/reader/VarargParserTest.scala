package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class VarargParserTest extends FunSuite {

  test("parse function call with single vararg") {
    val input = "func(1, 2, xs*)"
    val expected =
      FunctionCall(
        function = Identifier("func", meta = None),
        telescope = Tuple(
          terms = Vector(
            IntegerLiteral(1, meta = None),
            IntegerLiteral(2, meta = None),
            OpSeq(
              seq = Vector(
                Identifier("xs", meta = None),
                Identifier("*", meta = None)
              ),
              meta = None
            )
          ),
          meta = None
        ),
        meta = None
      )
    parseAndCheckV1(input, expected)
  }

  test("parse function def with single vararg") {
    val input = "func(x : Integer *)"
    val expected =
      FunctionCall(
        function = Identifier("func", meta = None),
        telescope = Tuple(
          terms = Vector(
            OpSeq(
              seq = Vector(
                Identifier("x", meta = None),
                Identifier(":", meta = None),
                Identifier("Integer", meta = None),
                Identifier("*", meta = None)
              ),
              meta = None
            )
          ),
          meta = None
        ),
        meta = None
      )
    parseAndCheckV1(input, expected)
  }

}
