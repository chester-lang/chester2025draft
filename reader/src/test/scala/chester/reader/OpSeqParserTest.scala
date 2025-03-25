package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class OpSeqParserTest extends FunSuite {

  test("parse simple opSeq with single operator") {
    val input = "1 + 2"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse simple opSeq with single operator 2") {
    val input = "1 *2"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("*", meta = None),
        IntegerLiteral(2, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with multiple operators") {
    val input = "1 + 2 + 3 + 4"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(3, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(4, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with mixed operators and precedence") {
    val input = "1 + 2 * 4 + 5"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None),
        Identifier("*", meta = None),
        IntegerLiteral(4, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(5, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse prefix") {
    val input = "+ x"
    val expected = OpSeq(
      Vector(
        Identifier("+", meta = None),
        Identifier("x", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse prefix2") {
    val input = "not x"
    val expected = OpSeq(
      Vector(
        Identifier("not", meta = None),
        Identifier("x", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse mixfix") {
    val input = "if x then q else w"
    val expected = OpSeq(
      Vector(
        Identifier("if", meta = None),
        Identifier("x", meta = None),
        Identifier("then", meta = None),
        Identifier("q", meta = None),
        Identifier("else", meta = None),
        Identifier("w", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse mixfix2") {
    val input = "if x then f(if o then a else b) else w"
    val expected = OpSeq(
      Vector(
        Identifier("if", meta = None),
        Identifier("x", meta = None),
        Identifier("then", meta = None),
        FunctionCall(
          function = Identifier("f", meta = None),
          telescope = Tuple(
            Vector(
              OpSeq(
                Vector(
                  Identifier("if", meta = None),
                  Identifier("o", meta = None),
                  Identifier("then", meta = None),
                  Identifier("a", meta = None),
                  Identifier("else", meta = None),
                  Identifier("b", meta = None)
                ),
                meta = None
              )
            ),
            meta = None
          ),
          meta = None
        ),
        Identifier("else", meta = None),
        Identifier("w", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with mixed operators without spaces") {
    val input = "1+2*4+5"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(2, meta = None),
        Identifier("*", meta = None),
        IntegerLiteral(4, meta = None),
        Identifier("+", meta = None),
        IntegerLiteral(5, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with not") {
    val input = "!1"
    val expected = OpSeq(
      Vector(
        Identifier("!", meta = None),
        IntegerLiteral(1, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with and") {
    val input = "1 and 5"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("and", meta = None),
        IntegerLiteral(5, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse val input") {
    val input = "val input = \"1 -> 5\""
    val expected = OpSeq(
      Vector(
        Identifier("val", meta = None),
        Identifier("input", meta = None),
        Identifier("=", meta = None),
        StringLiteral("1 -> 5", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse opSeq with ->") {
    val input = "1 -> 5"
    val expected = OpSeq(
      Vector(
        IntegerLiteral(1, meta = None),
        Identifier("->", meta = None),
        IntegerLiteral(5, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

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
    parseAndCheckBoth(input, expected)
  }

  test("parse function call with") {
    val input = "+(2 + 3)"
    val expected = FunctionCall(
      function = Identifier("+", meta = None),
      telescope = Tuple(
        Vector(
          OpSeq(
            Vector(
              IntegerLiteral(2, meta = None),
              Identifier("+", meta = None),
              IntegerLiteral(3, meta = None)
            ),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("some macro") {
    val input =
      "def apply(heads: Vector[Expr], tail: Expr): Block = Block(heads, Some(tail), None)"
    val expected = OpSeq(
      Vector(
        Identifier("def", meta = None),
        FunctionCall(
          function = Identifier("apply", meta = None),
          telescope = Tuple(
            Vector(
              OpSeq(
                Vector(
                  Identifier("heads", meta = None),
                  Identifier(":", meta = None),
                  FunctionCall(
                    function = Identifier("Vector", meta = None),
                    telescope = ListExpr(
                      Vector(Identifier("Expr", meta = None)),
                      meta = None
                    ),
                    meta = None
                  )
                ),
                meta = None
              ),
              OpSeq(
                Vector(
                  Identifier("tail", meta = None),
                  Identifier(":", meta = None),
                  Identifier("Expr", meta = None)
                ),
                meta = None
              )
            ),
            meta = None
          ),
          meta = None
        ),
        Identifier(":", meta = None),
        Identifier("Block", meta = None),
        Identifier("=", meta = None),
        FunctionCall(
          function = Identifier("Block", meta = None),
          telescope = Tuple(
            Vector(
              Identifier("heads", meta = None),
              FunctionCall(
                function = Identifier("Some", meta = None),
                telescope = Tuple(Vector(Identifier("tail", meta = None)), meta = None),
                meta = None
              ),
              Identifier("None", meta = None)
            ),
            meta = None
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("more macro") {
    val input = "def apply(heads: Vector[Expr], tail: Expr): Block"
    val expected = OpSeq(
      Vector(
        Identifier("def", meta = None),
        FunctionCall(
          function = Identifier("apply", meta = None),
          telescope = Tuple(
            Vector(
              OpSeq(
                Vector(
                  Identifier("heads", meta = None),
                  Identifier(":", meta = None),
                  FunctionCall(
                    function = Identifier("Vector", meta = None),
                    telescope = ListExpr(
                      Vector(Identifier("Expr", meta = None)),
                      meta = None
                    ),
                    meta = None
                  )
                ),
                meta = None
              ),
              OpSeq(
                Vector(
                  Identifier("tail", meta = None),
                  Identifier(":", meta = None),
                  Identifier("Expr", meta = None)
                ),
                meta = None
              )
            ),
            meta = None
          ),
          meta = None
        ),
        Identifier(":", meta = None),
        Identifier("Block", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse Identifier to IntegerLiteral") {
    val input = "Identifier(\"b\") -> IntegerLiteral(2)"
    val expected = OpSeq(
      Vector(
        FunctionCall(
          function = Identifier("Identifier", meta = None),
          telescope = Tuple(
            Vector(StringLiteral("b", meta = None)),
            meta = None
          ),
          meta = None
        ),
        Identifier("->", meta = None),
        FunctionCall(
          function = Identifier("IntegerLiteral", meta = None),
          telescope = Tuple(
            Vector(IntegerLiteral(2, meta = None)),
            meta = None
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse OpSeq with identifier to integer literal mapping") {
    val input = "Vector(\n  Identifier(\"b\") -> IntegerLiteral(2)\n)"
    val expected = FunctionCall(
      function = Identifier("Vector", meta = None),
      telescope = Tuple(
        Vector(
          OpSeq(
            Vector(
              FunctionCall(
                function = Identifier("Identifier", meta = None),
                telescope = Tuple(
                  Vector(StringLiteral("b", meta = None)),
                  meta = None
                ),
                meta = None
              ),
              Identifier("->", meta = None),
              FunctionCall(
                function = Identifier("IntegerLiteral", meta = None),
                telescope = Tuple(
                  Vector(IntegerLiteral(2, meta = None)),
                  meta = None
                ),
                meta = None
              )
            ),
            meta = None
          )
        ),
        meta = None
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }
  test("parse case pattern with underscore and block") {
    val input = "case Email(sender, title, _) => { println(sender) }"
    val expected = OpSeq(
      Vector(
        Identifier("case", None),
        FunctionCall(
          Identifier("Email", None),
          Tuple(
            Vector(
              Identifier("sender", None),
              Identifier("title", None),
              Identifier("_", None)
            ),
            None
          ),
          None
        ),
        Identifier("=>", None),
        Block(
          Vector(),
          Some(
            FunctionCall(
              Identifier("println", None),
              Tuple(
                Vector(
                  Identifier("sender", None)
                ),
                None
              ),
              None
            )
          ),
          None
        )
      ),
      None
    )
    parseAndCheck(input, expected)
  }

}
