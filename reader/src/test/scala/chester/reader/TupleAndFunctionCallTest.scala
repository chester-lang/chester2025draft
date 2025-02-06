package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class TupleAndFunctionCallTest extends FunSuite {
  test("parse tuple with type annotation") {
    val input = "(a: Integer)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier(":", meta = None),
            Identifier("Integer", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse function call") {
    val input = "Identifier(\"b\")"
    val expected = FunctionCall(
      Identifier("Identifier", meta = None),
      Tuple(
        Vector(
          StringLiteral("b", meta = None)
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse identifier to function call conversion") {
    val input = "Identifier(\"b\") -> IntegerLiteral(2)"
    val expected = OpSeq(
      Vector(
        FunctionCall(
          Identifier("Identifier", meta = None),
          Tuple(
            Vector(
              StringLiteral("b", meta = None)
            ),
            meta = None
          ),
          meta = None
        ),
        Identifier("->", meta = None),
        FunctionCall(
          Identifier("IntegerLiteral", meta = None),
          Tuple(
            Vector(
              IntegerLiteral(2, meta = None)
            ),
            meta = None
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }
}
