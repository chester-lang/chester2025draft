package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class SpaceAndNewlineParserTest extends FunSuite {
  test("function call without space before parentheses") {
    val input = "f() + g()"
    val expected = OpSeq(
      Vector(
        FunctionCall(
          Identifier("f", meta = None),
          Tuple(Vector.empty, meta = None),
          meta = None
        ),
        Identifier("+", meta = None),
        FunctionCall(
          Identifier("g", meta = None),
          Tuple(Vector.empty, meta = None),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("function call with space before parentheses") {
    val input = "f () + g ()"
    val expected = OpSeq(
      Vector(
        Identifier("f", meta = None),
        Tuple(Vector.empty, meta = None),
        Identifier("+", meta = None),
        Identifier("g", meta = None),
        Tuple(Vector.empty, meta = None)
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("newline after block ends expression") {
    val input = """f {
      1
    }
    g()"""
    val expected = Block(
      Vector(
        FunctionCall(
          Identifier("f", meta = None),
          Tuple(
            Vector(
              Block(
                Vector.empty,
                Some(IntegerLiteral(1, meta = None)),
                meta = None
              )
            ),
            meta = None
          ),
          meta = None
        ),
        FunctionCall(
          Identifier("g", meta = None),
          Tuple(Vector.empty, meta = None),
          meta = None
        )
      ),
      None,
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("newline within block is not significant") {
    val input = """f {
      1
      + 2
    }"""
    val expected = FunctionCall(
      Identifier("f", meta = None),
      Tuple(
        Vector(
          Block(
            Vector.empty,
            Some(
              OpSeq(
                Vector(
                  IntegerLiteral(1, meta = None),
                  Identifier("+", meta = None),
                  IntegerLiteral(2, meta = None)
                ),
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
} 