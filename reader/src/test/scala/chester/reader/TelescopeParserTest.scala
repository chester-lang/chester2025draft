package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class TelescopeParserTest extends FunSuite {
  test("parse tuple with simple arguments") {
    val input = "(a, b, c)"
    val expected = Tuple(
      Vector(
        Identifier("a", meta = None),
        Identifier("b", meta = None),
        Identifier("c", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with simple arguments ending comma") {
    val input = "(a, b, c,)"
    val expected = Tuple(
      Vector(
        Identifier("a", meta = None),
        Identifier("b", meta = None),
        Identifier("c", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple empty") {
    val input = "()"
    val expected = Tuple(Vector(), meta = None)
    parseAndCheckBoth(input, expected)
  }

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
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments having type") {
    val input = "(a: Integer)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier(":", meta = None),
            Identifier("Integer", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments having types") {
    val input = "(a: Integer, b: String, c: Double)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier(":", meta = None),
            Identifier("Integer", meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("b", meta = None),
            Identifier(":", meta = None),
            Identifier("String", meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("c", meta = None),
            Identifier(":", meta = None),
            Identifier("Double", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments having default value") {
    val input = "(a = 1)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier("=", meta = None),
            IntegerLiteral(1, meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments having default values") {
    val input = "(a = 1, b = 2, c = 3)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier("=", meta = None),
            IntegerLiteral(1, meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("b", meta = None),
            Identifier("=", meta = None),
            IntegerLiteral(2, meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("c", meta = None),
            Identifier("=", meta = None),
            IntegerLiteral(3, meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments having types and default values") {
    val input = "(a: Integer = 1, b: String = \"test\", c: Double = 3.14)"
    val expected = Tuple(
      Vector(
        OpSeq(
          Vector(
            Identifier("a", meta = None),
            Identifier(":", meta = None),
            Identifier("Integer", meta = None),
            Identifier("=", meta = None),
            IntegerLiteral(1, meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("b", meta = None),
            Identifier(":", meta = None),
            Identifier("String", meta = None),
            Identifier("=", meta = None),
            StringLiteral("test", meta = None)
          ),
          meta = None
        ),
        OpSeq(
          Vector(
            Identifier("c", meta = None),
            Identifier(":", meta = None),
            Identifier("Double", meta = None),
            Identifier("=", meta = None),
            RationalLiteral(BigDecimal(3.14), meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse tuple with arguments without names") {
    val input = "(1, 2, 3)"
    val expected = Tuple(
      Vector(
        IntegerLiteral(1, meta = None),
        IntegerLiteral(2, meta = None),
        IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }

  test("parse generics with arguments without names") {
    val input = "[Integer]"
    val expected = ListExpr(
      Vector(
        Identifier("Integer", meta = None)
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }
}
