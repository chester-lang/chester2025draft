package chester.tyck

import chester.error.*
import chester.syntax.concrete.*
import chester.syntax.core.{*, given}
import chester.syntax.core.truffle.*
import chester.tyck.api.*
import munit.FunSuite

class LazyReductionTest extends FunSuite {
  val tycker = Tycker

  test("field access - no reduction needed for direct record") {
    val recordStmt = RecordStmt(
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

    val recordExpr = FunctionCall(
      Identifier("Record", None),
      Tuple(Vector(IntegerLiteral(42, None)), None),
      None
    )
    val fieldAccessExpr = DotCall(recordExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(fieldAccessExpr)
    result match {
      case TyckResult.Success(judge, _, _) =>
        assertEquals(judge.ty, IntegerType(None))
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Type checking failed with errors: $errors")
    }
  }

  test("field access - lazy reduction of function call") {
    val recordStmt = RecordStmt(
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

    val functionExpr = DesaltFunctionCall(
      FunctionExpr(
        Vector(DefTelescope(Vector(), false, None)),
        Some(Identifier("Type", None)),
        None,
        FunctionCall(
          Identifier("Record", None),
          Tuple(Vector(IntegerLiteral(42, None)), None),
          None
        ),
        None
      ),
      Vector(DesaltCallingTelescope(Vector(), false, None)),
      None
    )

    val fieldAccessExpr = DotCall(functionExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(fieldAccessExpr)
    result match {
      case TyckResult.Success(judge, _, _) =>
        assertEquals(judge.ty, IntegerType(None))
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Type checking failed with errors: $errors")
    }
  }

  test("field access - lazy reduction of type-level function") {
    val functionExpr = DesaltFunctionCall(
      FunctionExpr(
        Vector(DefTelescope(Vector(), false, None)),
        Some(Identifier("Type", None)),
        None,
        IntegerLiteral(42, None),
        None
      ),
      Vector(DesaltCallingTelescope(Vector(), false, None)),
      None
    )
    val fieldAccessExpr = DotCall(functionExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(fieldAccessExpr)
    result match {
      case TyckResult.Success(judge, _, _) =>
        assertEquals(judge.ty, IntegerType(None))
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Type checking failed with errors: $errors")
    }
  }

  test("field access - lazy reduction of type-level function with type result") {
    val functionExpr = DesaltFunctionCall(
      FunctionExpr(
        Vector(DefTelescope(Vector(), false, None)),
        Some(Identifier("Type", None)),
        None,
        Identifier("Type", None),
        None
      ),
      Vector(DesaltCallingTelescope(Vector(), false, None)),
      None
    )
    val fieldAccessExpr = DotCall(functionExpr, Identifier("a", None), Vector(), None)
    
    val result = tycker.check(fieldAccessExpr)
    result match {
      case TyckResult.Success(judge, _, _) =>
        assertEquals(judge.ty, Type(Level0, None))
      case TyckResult.Failure(errors, _, _, _) =>
        fail(s"Type checking failed with errors: $errors")
    }
  }
} 