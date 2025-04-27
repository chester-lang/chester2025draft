package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class HardObjectTest  extends FunSuite {

  test("{ boot.loader.systemd-boot.enable = true, y = 0, }") {
    val input = "{ boot.loader.systemd-boot.enable = true, y = 0, }"
    val expected =
      ObjectExpr(
        clauses = Vector(
          ObjectExprClause(
            key = DotCall(
              expr = DotCall(
                expr = DotCall(
                  expr = Identifier(
                    name = "boot",
                    meta = None
                  ),
                  field = Identifier(
                    name = "loader",
                    meta = None
                  ),
                  telescope = Vector(),
                  meta = None
                ),
                field = Identifier(
                  name = "systemd-boot",
                  meta = None
                ),
                telescope = Vector(),
                meta = None
              ),
              field = Identifier(
                name = "enable",
                meta = None
              ),
              telescope = Vector(),
              meta = None
            ),
            value = Identifier(
              name = "true",
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
