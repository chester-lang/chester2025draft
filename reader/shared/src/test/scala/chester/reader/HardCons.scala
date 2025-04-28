package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class HardCons extends FunSuite {

  test("Cons { head = 0, tail = Nil }") {
    val input = "Cons { head = 0, tail = Nil }"
    val expected =
      FunctionCall(
        function = Identifier(
          name = "Cons",
          meta = None
        ),
        telescope = Tuple(
          terms = Vector(
            ObjectExpr(
              clauses = Vector(
                ObjectExprClause(
                  key = Identifier(
                    name = "head",
                    meta = None
                  ),
                  value = IntegerLiteral(
                    value = 0,
                    meta = None
                  )
                ),
                ObjectExprClause(
                  key = Identifier(
                    name = "tail",
                    meta = None
                  ),
                  value = Identifier(
                    name = "Nil",
                    meta = None
                  )
                )
              ),
              meta = None
            )
          ),
          meta = None
        ),
        meta = None
      )
    parseAndCheckBoth(input, expected)
  }

}
