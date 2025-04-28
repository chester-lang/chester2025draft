package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class CustomOperators extends FunSuite {

  test("?hole") {
    val input = "?hole"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "?",
            meta = None
          ),
          Identifier(
            name = "hole",
            meta = None
          )
        ),
        meta = None
      )
    parseAndCheckBoth(input, expected)
  }

  test("{proof1 = ?hole;}") {
    val input = "{proof1 = ?hole;}"
    val expected =
      Block(
        statements = Vector(
          OpSeq(
            seq = Vector(
              Identifier(
                name = "proof1",
                meta = None
              ),
              Identifier(
                name = "=",
                meta = None
              ),
              Identifier(
                name = "?",
                meta = None
              ),
              Identifier(
                name = "hole",
                meta = None
              )
            ),
            meta = None
          )
        ),
        result = None,
        meta = None
      )
    parseAndCheckBoth(input, expected)
  }

}
