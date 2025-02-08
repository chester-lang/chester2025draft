package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.*
import chester.tyck.api.*
import munit.FunSuite

class LazyReductionTest extends FunSuite {
  val tycker = Tycker

  // Helper method to set up common record definition
  private def createRecordBlock(fieldType: Option[Identifier] = Some(Identifier("Integer", None))) = Block(
    Vector(
      RecordStmt(
        Identifier("Record", None),
        Vector(
          RecordField(
            Identifier("a", None),
            fieldType,
            None,
            None
          )
        ),
        None,
        None,
        None
      )
    ),
    Some(UnitExpr(None)),
    None
  )

  test("field access - no reduction needed for direct record") {
    val recordBlock = Block(
      Vector(
        RecordStmt(
          Identifier("Record", None),
          Vector(
            RecordField(
              Identifier("a", None),
              Some(Identifier("Integer", None)),
              None,
              None
            )
          ),
          None,
          None,
          None
        )
      ),
      Some(DotCall(
        FunctionCall(
          Identifier("Record", None),
          Tuple(Vector(IntegerLiteral(42, None)), None),
          None
        ),
        Identifier("a", None),
        Vector(),
        None
      )),
      None
    )
    
    val result = tycker.check(recordBlock)
    result match {
      case TyckResult.Success(judge, _, _) =>
        assertEquals(judge.ty, IntegerType(None))
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Type checking failed with errors: $errors")
      case _ => fail("Unexpected result type")
    }
  }

  test("field access - lazy reduction of function call") {
    val recordBlock = createRecordBlock()
    val recordExpr = FunctionCall(
      Identifier("Record", None),
      Tuple(Vector(IntegerLiteral(42, None)), None),
      None
    )
    val functionExpr = DesaltFunctionCall(
      FunctionExpr(
        Vector(DefTelescope(Vector(), false, None)),
        Some(Identifier("Type", None)),
        None,
        recordExpr,
        None
      ),
      Vector(DesaltCallingTelescope(Vector(), false, None)),
      None
    )
    val fieldAccessExpr = DotCall(functionExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(recordBlock)
    result match {
      case TyckResult.Success(_, _, _) =>
        val accessResult = tycker.check(fieldAccessExpr)
        accessResult match {
          case TyckResult.Success(judge, _, _) =>
            assertEquals(judge.ty, IntegerType(None))
          case TyckResult.Failure(errors, _, _, _) =>
            fail(s"Type checking failed with errors: $errors")
          case _ => fail("Unexpected result type")
        }
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Record definition failed with errors: $errors")
      case _ => fail("Unexpected result type")
    }
  }

  test("field access - lazy reduction of type-level function with type result") {
    val recordBlock = createRecordBlock(Some(Identifier("Type", None)))
    val recordExpr = FunctionCall(
      Identifier("Record", None),
      Tuple(Vector(Identifier("Type", None)), None),
      None
    )
    val functionExpr = DesaltFunctionCall(
      FunctionExpr(
        Vector(DefTelescope(Vector(), false, None)),
        Some(Identifier("Type", None)),
        None,
        recordExpr,
        None
      ),
      Vector(DesaltCallingTelescope(Vector(), false, None)),
      None
    )
    val fieldAccessExpr = DotCall(functionExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(recordBlock)
    result match {
      case TyckResult.Success(_, _, _) =>
        val accessResult = tycker.check(fieldAccessExpr)
        accessResult match {
          case TyckResult.Success(judge, _, _) =>
            assertEquals(judge.ty, Type(Level0, None))
          case TyckResult.Failure(errors, _, _, _) =>
            fail(s"Type checking failed with errors: $errors")
          case _ => fail("Unexpected result type")
        }
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Record definition failed with errors: $errors")
      case _ => fail("Unexpected result type")
    }
  }
} 