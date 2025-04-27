package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.readerv2.LexerV2.DEBUG

class ConsVectorTest extends FunSuite {
  test("Cons extending Vector with head and tail") {
    val input = "Cons[n, T] <: Vector[succ(n), T] { head: T; tail: Vector[n, T]; }"
    
    // Run with V1 parser and print the result with pprint
    val result = parseV1(input)
    pprint.pprintln(result, width = 80, height = 1000)
    
    val expected = OpSeq(
      seq = Vector(
        FunctionCall(
          function = Identifier(
            name = "Cons",
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
        ),
        Identifier(
          name = "<:",
          meta = None
        ),
        FunctionCall(
          function = Identifier(
            name = "Vector",
            meta = None
          ),
          telescope = ListExpr(
            terms = Vector(
              FunctionCall(
                function = Identifier(
                  name = "succ",
                  meta = None
                ),
                telescope = Tuple(
                  terms = Vector(
                    Identifier(
                      name = "n",
                      meta = None
                    )
                  ),
                  meta = None
                ),
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
    
    DEBUG.withValue(false) {
      parseAndCheckV1(input, expected)
    }
  }
} 