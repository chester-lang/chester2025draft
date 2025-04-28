package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class HardTableTest extends FunSuite {

  test("{ x.x = 1, y = 0 }") {
    val input = "{ x.x = 1, y = 0 }"
    val expected =
      ObjectExpr(
        clauses = Vector(
          ObjectExprClause(
            key = DotCall(
              expr = Identifier(
                name = "x",
                meta = None
              ),
              field = Identifier(
                name = "x",
                meta = None
              ),
              telescope = Vector(),
              meta = None
            ),
            value = IntegerLiteral(
              value = 1,
              meta = None
            )
          ),
          ObjectExprClause(
            key = Identifier(
              name = "y",
              meta = None
            ),
            value = IntegerLiteral(
              value = 0,
              meta = None
            )
          )
        ),
        meta = None
      )
    parseAndCheckBoth(input, expected)
  }

}
