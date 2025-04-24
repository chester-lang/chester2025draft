package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class HardOpSeqParserTest extends FunSuite {

  test("parse infix with block") {
    val input = "so getthen { doSomething }"
    val expected = OpSeq(
      Vector(
        Identifier("so", meta = None),
        Identifier("getthen", meta = None),
        Block(
          heads = Vector(),
          tail = Identifier("doSomething", meta = None),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

}
