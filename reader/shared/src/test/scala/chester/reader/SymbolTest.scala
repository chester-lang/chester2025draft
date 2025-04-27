package chester.reader

import chester.syntax.concrete.Identifier
import munit.FunSuite

class SymbolTest  extends FunSuite{

  test("systemd-sth") {
    val input = "systemd-sth"
    val expected =
      Identifier(
        name = "systemd-sth",
        meta = None
      )
    parseAndCheckV1(input, expected)
  }

}
