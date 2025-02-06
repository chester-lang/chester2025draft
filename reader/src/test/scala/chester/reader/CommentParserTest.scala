package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class CommentParserTest extends FunSuite {
  test("parse tuple with simple arguments with comments") {
    val input = "(a, // comment \n b, c)"
    val expected = Tuple(
      Vector(
        Identifier("a", meta = None),
        Identifier("b", meta = None),
        Identifier("c", meta = None)
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }
}
