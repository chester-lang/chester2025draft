package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class MoreOpSeqTest extends FunSuite {

  test("parse a @") {
    val input = "@derive(Show)"
    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "@",
            meta = None
          ),
          FunctionCall(
            function = Identifier(
              name = "derive",
              meta = None
            ),
            telescope = Tuple(
              terms = Vector(
                Identifier(
                  name = "Show",
                  meta = None
                )
              ),
              meta = None
            ),
            meta = None
          )
        ),
        meta = None
      )
    parseAndCheckV1(input, expected)
  }

}
