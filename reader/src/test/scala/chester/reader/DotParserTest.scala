package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class DotParserTest extends FunSuite {
  test("world.execute(me)") {
    val input = "world.execute(me)"
    val expected = DotCall(
      Identifier("world", meta = None),
      Identifier("execute", meta = None),
      Vector(
        Tuple(
          Vector(
            Identifier("me", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse simple dot call") {
    val input = "obj.field"
    val expected = DotCall(
      Identifier("obj", meta = None),
      Identifier("field", meta = None),
      Vector(),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot call with function call") {
    val input = "obj.method()"
    val expected = DotCall(
      Identifier("obj", meta = None),
      Identifier("method", meta = None),
      Vector(
        Tuple(
          Vector(),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse nested dot calls") {
    val input = "obj.field1.field2"
    val expected = DotCall(
      DotCall(
        Identifier("obj", meta = None),
        Identifier("field1", meta = None),
        Vector(),
        meta = None
      ),
      Identifier("field2", meta = None),
      Vector(),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot call with arguments") {
    val input = "obj.method(arg1, arg2)"
    val expected = DotCall(
      Identifier("obj", meta = None),
      Identifier("method", meta = None),
      Vector(
        Tuple(
          Vector(
            Identifier("arg1", meta = None),
            Identifier("arg2", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot call with arguments arguments") {
    val input = "obj.method(arg1, arg2)(arg1)"
    val expected = DotCall(
      expr = Identifier("obj", meta = None),
      field = Identifier("method", meta = None),
      telescope = Vector(
        Tuple(
          terms = Vector(
            Identifier("arg1", meta = None),
            Identifier("arg2", meta = None)
          ),
          meta = None
        ),
        Tuple(
          terms = Vector(
            Identifier("arg1", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot call with arguments block arguments") {
    val input = "obj.method(arg1, arg2){arg1}"
    val expected = DotCall(
      Identifier("obj", meta = None),
      Identifier("method", meta = None),
      Vector(
        Tuple(
          Vector(
            Identifier("arg1", meta = None),
            Identifier("arg2", meta = None)
          ),
          meta = None
        ),
        Tuple(
          Vector(
            Block(
              statements = Vector(),
              result = Some(Identifier("arg1", meta = None)),
              meta = None
            )
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot + call with arguments") {
    val input = "obj.+(arg1, arg2)"
    val expected = DotCall(
      Identifier("obj", meta = None),
      Identifier("+", meta = None),
      Vector(
        Tuple(
          Vector(
            Identifier("arg1", meta = None),
            Identifier("arg2", meta = None)
          ),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }

  test("parse dot call followed by function call") {
    val input = "obj.method().anotherMethod()"
    val expected = DotCall(
      DotCall(
        Identifier("obj", meta = None),
        Identifier("method", meta = None),
        Vector(
          Tuple(
            Vector(),
            meta = None
          )
        ),
        meta = None
      ),
      Identifier("anotherMethod", meta = None),
      Vector(
        Tuple(
          Vector(),
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckV1(input, expected)
  }
}
