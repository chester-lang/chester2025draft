package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class KeywordTest extends FunSuite {

  test("parse a keyword") {
    val input = "#\uD83D\uDE3F\uD83D\uDE3F"
    val expected = Keyword(
      key = "\ud83d\ude3f\ud83d\ude3f",
      telescope = Vector(),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse a keyword with arguments") {
    val input = "#qwq(1,2,3)"
    val expected = Keyword(
      key = "qwq",
      telescope = Vector(
        Tuple(
          Vector(
            IntegerLiteral(1, meta = None),
            IntegerLiteral(2, meta = None),
            IntegerLiteral(3, meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

}
