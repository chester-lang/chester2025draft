package chester.parser

import chester.parser.*
import chester.syntax.concrete.*
import munit.FunSuite

class BlockAndBlockCallParserTest extends FunSuite {
  test("parse block with multiple statements without newlines") {
    val input = "{f(1); g(2); 3}"
    val expected = Block(
      Vector(
        FunctionCall(
          Identifier("f", meta = None),
          Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
          meta = None
        ),
        FunctionCall(
          Identifier("g", meta = None),
          Tuple(Vector(IntegerLiteral(2, meta = None)), meta = None),
          meta = None
        )
      ),
      Some(IntegerLiteral(3, meta = None)),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse block with multiple statements with newlines") {
    val input = "{\n  f(1);\n  g(2);\n  3\n}"
    val expected = Block(
      Vector(
        FunctionCall(
          Identifier("f", meta = None),
          Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
          meta = None
        ),
        FunctionCall(
          Identifier("g", meta = None),
          Tuple(Vector(IntegerLiteral(2, meta = None)), meta = None),
          meta = None
        )
      ),
      Some(IntegerLiteral(3, meta = None)),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse block call with nested block without newlines") {
    val input = "f{g(1); 2}"
    val expected = FunctionCall(
      Identifier("f", meta = None),
      Tuple(
        Vector(
          Block(
            Vector(
              FunctionCall(
                Identifier("g", meta = None),
                Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
                meta = None
              )
            ),
            Some(IntegerLiteral(2, meta = None)),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse block call with nested block with newlines") {
    val input = "f {\n  g(1);\n  2\n}"
    val expected = FunctionCall(
      Identifier("f", meta = None),
      Tuple(
        Vector(
          Block(
            Vector(
              FunctionCall(
                Identifier("g", meta = None),
                Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
                meta = None
              )
            ),
            Some(IntegerLiteral(2, meta = None)),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse function call with block as argument without newlines") {
    val input = "f(1) {2}"
    val expected = FunctionCall(
      FunctionCall(
        Identifier("f", meta = None),
        Tuple(Vector(IntegerLiteral(1, meta = None)), meta = None),
        meta = None
      ),
      Tuple(
        Vector(
          Block(
            Vector(),
            Some(IntegerLiteral(2, meta = None)),
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
