package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.reader.parseAndCheckTopLevelBoth
import chester.reader.parseTopLevelV1
import chester.reader.parseTopLevelV2
import chester.i18n.t

class TopLevelParserTest extends FunSuite {
  test("parse simple expression as top level") {
    val input = "2 + 3"
    val expected = Block(
      statements = Vector.empty,
      result = Some(
        OpSeq(
          seq = Vector(
            IntegerLiteral(value = 2, meta = None),
            Identifier(name = "+", meta = None),
            IntegerLiteral(value = 3, meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckTopLevelBoth(input, expected)
  }

  test("parse variable declaration as top level") {
    val input = "let x = 42;"
    val expected = Block(
      statements = Vector(
        OpSeq(
          seq = Vector(
            Identifier(name = "let", meta = None),
            Identifier(name = "x", meta = None),
            Identifier(name = "=", meta = None),
            IntegerLiteral(value = 42, meta = None)
          ),
          meta = None
        )
      ),
      result = None,
      meta = None
    )
    parseAndCheckTopLevelBoth(input, expected)
  }

  test("parse function declaration as top level") {
    val input = "def add(x, y) = x + y;"
    val expected = Block(
      statements = Vector(
        OpSeq(
          seq = Vector(
            Identifier(name = "def", meta = None),
            FunctionCall(
              function = Identifier(name = "add", meta = None),
              telescope = Tuple(
                terms = Vector(
                  Identifier(name = "x", meta = None),
                  Identifier(name = "y", meta = None)
                ),
                meta = None
              ),
              meta = None
            ),
            Identifier(name = "=", meta = None),
            Identifier(name = "x", meta = None),
            Identifier(name = "+", meta = None),
            Identifier(name = "y", meta = None)
          ),
          meta = None
        )
      ),
      result = None,
      meta = None
    )
    parseAndCheckTopLevelBoth(input, expected)
  }

  test("parse multiple top level statements") {
    val input = """
      let x = 1;
      let y = 2;
      let z = x + y;
    """
    val result1 = parseTopLevelV1(input)
    val result2 = parseTopLevelV2(input)

    assertEquals(result1, result2, t"V1 and V2 parsers should produce the same result")
  }
}
