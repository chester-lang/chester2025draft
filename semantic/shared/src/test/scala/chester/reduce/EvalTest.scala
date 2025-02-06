package chester.reduce

import chester.syntax.core.*
import chester.syntax.core.truffle.*
import chester.uniqid.*
import munit.FunSuite

class EvalTest extends FunSuite {
  given ctx: ReduceContext = ReduceContext()
  given reducer: Reducer = NaiveReducer

  test("basic reduction") {
    val functionType = FunctionType(Vector(), IntegerType(None), Effects.Empty, None)
    val result = reducer.reduce(
      FCallTerm(
        Function(
          functionType,
          IntegerType(None),
          None
        ),
        Vector(Calling(Vector(), false, None)),
        None
      )
    )
    assertEquals(result, IntegerType(None))
  }

  test("function application with arguments") {
    val x = LocalV("x", IntegerType(None), Uniqid.generate[LocalV], None)
    val functionType = FunctionType(
      Vector(TelescopeTerm(Vector(ArgTerm(x, IntegerType(None), None, false, None)), false, None)),
      IntegerType(None),
      Effects.Empty,
      None
    )
    val result = reducer.reduce(
      FCallTerm(
        Function(
          functionType,
          x,
          None
        ),
        Vector(Calling(Vector(CallingArgTerm(AbstractIntTerm_.from(5, None), IntegerType(None), None, false, None)), false, None)),
        None
      )
    )
    assertEquals(result, AbstractIntTerm_.from(5, None))
  }

  test("type-level computation") {
    val result = reducer.reduce(
      RecordStmtTerm(
        "A",
        Uniqid.generate[RecordStmtTerm],
        Vector(FieldTerm("a", IntegerType(None), None)),
        None,
        None
      )
    )
    assertEquals(result, RecordStmtTerm(
      "A",
      result.asInstanceOf[RecordStmtTerm].uniqId,
      Vector(FieldTerm("a", IntegerType(None), None)),
      None,
      None
    ))
  }

  test("field access") {
    val result = reducer.reduce(
      FieldAccessTerm(
        RecordStmtTerm(
          "A",
          Uniqid.generate[RecordStmtTerm],
          Vector(FieldTerm("a", IntegerType(None), None)),
          None,
          None
        ),
        "a",
        IntegerType(None),
        None
      )
    )
    assertEquals(result, IntegerType(None))
  }
} 