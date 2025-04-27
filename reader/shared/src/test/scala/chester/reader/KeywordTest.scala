package chester.reader

import chester.syntax.concrete.Keyword
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

}
