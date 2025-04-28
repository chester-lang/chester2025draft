package chester.reader

import chester.syntax.concrete.*
import chester.readerv2.ReaderV2.DEBUG
import munit.FunSuite

class HardTopLevelTest2 extends FunSuite {
  test("cons2 = Cons { head = 0, tail = Nil };") {

    val input =
      """
        |cons2 = Cons { head = 0, tail = Nil };
        |""".stripMargin
    val expected =
      Block(
        statements = Vector(
          OpSeq(
            seq = Vector(
              Identifier(
                name = "cons2",
                meta = None
              ),
              Identifier(
                name = "=",
                meta = None
              ),
              Identifier(
                name = "Cons",
                meta = None
              ),
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
          )
        ),
        result = None,
        meta = None
      )
    DEBUG.withValue(false) {
      parseAndCheckTopLevelV1(input, expected)
    }
  }

}
