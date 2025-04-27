package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class CustomOperators extends FunSuite{

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

}
