package chester.reader

import chester.readerv2.ReaderV2.DEBUG
import munit.FunSuite
import chester.syntax.concrete.*

class ConsVectorTest2 extends FunSuite {
  test("block in opseq") {
    val input = "Cons haha { head: T; tail: Vector[n, T]; }"

    val expected =
      OpSeq(
        seq = Vector(
          Identifier(
            name = "Cons",
            meta = None
          ),
          Identifier(
            name = "haha",
            meta = None
          ),
          Block(
            statements = Vector(
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "head",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  Identifier(
                    name = "T",
                    meta = None
                  )
                ),
                meta = None
              ),
              OpSeq(
                seq = Vector(
                  Identifier(
                    name = "tail",
                    meta = None
                  ),
                  Identifier(
                    name = ":",
                    meta = None
                  ),
                  FunctionCall(
                    function = Identifier(
                      name = "Vector",
                      meta = None
                    ),
                    telescope = ListExpr(
                      terms = Vector(
                        Identifier(
                          name = "n",
                          meta = None
                        ),
                        Identifier(
                          name = "T",
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
            ),
            result = None,
            meta = None
          )
        ),
        meta = None
      )

    DEBUG.withValue(true) {
      parseAndCheckBoth(input, expected)
    }
  }
}
