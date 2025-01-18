package chester.reader

import chester.reader._
import chester.syntax.concrete._
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
    parseAndCheck(input, expected)
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
    parseAndCheck(input, expected)
  }

}
