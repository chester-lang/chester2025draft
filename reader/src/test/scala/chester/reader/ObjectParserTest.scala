package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class ObjectParserTest extends FunSuite {

  test("parse empty object") {
    val input = "{}"
    val expected = ObjectExpr(Vector(), meta = None)
    parseAndCheckV1(input, expected)
  }

  test("parse object with single field") {
    val input = "{ a = 1 }"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with single field 2") {
    val input = "{a=1}"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with single field 3") {
    val input = "{ 'a => 1}"
    val expected = ObjectExpr(
      Vector(
        ObjectExprClauseOnValue(
          SymbolLiteral("a", meta = None),
          IntegerLiteral(1, meta = None)
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with single field 4") {
    val input = "{'a=>1}"
    val expected = ObjectExpr(
      Vector(
        ObjectExprClauseOnValue(
          SymbolLiteral("a", meta = None),
          IntegerLiteral(1, meta = None)
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with multiple fields") {
    val input = "{ a = 1, b = 2, c = 3 }"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None),
        Identifier("b", meta = None) -> IntegerLiteral(2, meta = None),
        Identifier("c", meta = None) -> IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with multiple fields with one more comma") {
    val input = "{ a = 1, b = 2, c = 3 ,}"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None),
        Identifier("b", meta = None) -> IntegerLiteral(2, meta = None),
        Identifier("c", meta = None) -> IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with multiple fields with one more comma without spaces") {
    val input = "{a=1,b=2,c=3,}"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None),
        Identifier("b", meta = None) -> IntegerLiteral(2, meta = None),
        Identifier("c", meta = None) -> IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with nested object") {
    val input = "{ a = { b = 2 }, c = 3 }"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> ObjectExpr(
          Vector(
            Identifier("b", meta = None) -> IntegerLiteral(2, meta = None)
          ),
          meta = None
        ),
        Identifier("c", meta = None) -> IntegerLiteral(3, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with mixed types") {
    val input = "{ a = 1, b = \"hello\", c = [1, 2, 3], d = { e = 4 } }"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None),
        Identifier("b", meta = None) -> StringLiteral("hello", meta = None),
        Identifier("c", meta = None) -> ListExpr(
          Vector(
            IntegerLiteral(1, meta = None),
            IntegerLiteral(2, meta = None),
            IntegerLiteral(3, meta = None)
          ),
          meta = None
        ),
        Identifier("d", meta = None) -> ObjectExpr(
          Vector(
            Identifier("e", meta = None) -> IntegerLiteral(4, meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse object with trailing comma") {
    val input = "{ a = 1, b = 2, }"
    val expected = ObjectExpr(
      Vector(
        Identifier("a", meta = None) -> IntegerLiteral(1, meta = None),
        Identifier("b", meta = None) -> IntegerLiteral(2, meta = None)
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }
}
