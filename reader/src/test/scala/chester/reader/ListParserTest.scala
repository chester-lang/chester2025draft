package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class ListParserTest extends FunSuite {

  test("parse empty list") {
    val input = "[]"
    val expected = ListExpr(Vector(), meta = None)
    parseAndCheckBoth(input, expected)
  }

  test("parse list with single integer") {
    val input = "[123]"
    val expected = ListExpr(Vector(IntegerLiteral(123, meta = None)), meta = None)
    parseAndCheckBoth(input, expected)
  }

  test("parse list with multiple integers and trailing comma") {
    val input = "[1, 2, 3,]"
    val expected = ListExpr(
      Vector(
        IntegerLiteral(1, meta = None),
        IntegerLiteral(2, meta = None),
        IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse list with mixed types") {
    val input = "[1, \"string\", 3.14]"
    val expected = ListExpr(
      Vector(
        IntegerLiteral(1, meta = None),
        StringLiteral("string", meta = None),
        RationalLiteral(BigDecimal(3.14), meta = None)
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  test("parse nested list") {
    val input = "[1, [2, 3], [4, [5]]]"
    val expected = ListExpr(
      Vector(
        IntegerLiteral(1, meta = None),
        ListExpr(
          Vector(
            IntegerLiteral(2, meta = None),
            IntegerLiteral(3, meta = None)
          ),
          meta = None
        ),
        ListExpr(
          Vector(
            IntegerLiteral(4, meta = None),
            ListExpr(
              Vector(
                IntegerLiteral(5, meta = None)
              ),
              meta = None
            )
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }
}
