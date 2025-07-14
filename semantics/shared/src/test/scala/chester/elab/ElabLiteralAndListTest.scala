package chester.elab

import chester.error.{TyckProblem, VectorReporter}
import chester.elab.{DefaultElaborator, ElabOps, TypescriptPlatformInfo, platformInfo}
import chester.reader.FileNameAndContent
import chester.readerv1.ChesterReaderV1
import chester.syntax.core.*
import chester.elab.api.NoopSemanticCollector
import munit.FunSuite
import chester.error.reporterToEither

class ElabLiteralAndListTest extends FunSuite {
  test("integer literal typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the integer literal expression using FileNameAndContent
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("literal.chester", "1"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is an IntTerm
      // The new elaboration system correctly types small integers as IntTerm
      assert(judge.wellTyped.isInstanceOf[IntTerm], s"Expected IntTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // Check that the type is IntType
      assert(judge.ty.isInstanceOf[IntType], s"Expected IntType but got ${judge.ty.getClass.getSimpleName}")
    }
  }

  test("heterogeneous list typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the list expression with mixed types
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("list.chester", "[1, \"a\"]"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is a ListTerm
      assert(judge.wellTyped.isInstanceOf[ListTerm], s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // Get the list terms and verify their types
      val listTerm = judge.wellTyped.asInstanceOf[ListTerm]
      assertEquals(listTerm.terms.size, 2, "List should have 2 elements")

      // First element should be an int term (not IntegerTerm - the elaborator chooses more specific types)
      assert(listTerm.terms(0).isInstanceOf[IntTerm], s"First element should be IntTerm but got ${listTerm.terms(0).getClass.getSimpleName}")

      // Second element should be a string
      assert(listTerm.terms(1).isInstanceOf[StringTerm], s"Second element should be StringTerm but got ${listTerm.terms(1).getClass.getSimpleName}")

      // Check the list type structure
      assert(judge.ty.isInstanceOf[ListType], s"Expected ListType but got ${judge.ty.getClass.getSimpleName}")

      val listType = judge.ty.asInstanceOf[ListType]

      // Verify that the element type is a Union type
      assert(listType.ty.isInstanceOf[Union], s"Expected Union type for list elements but got ${listType.ty.getClass.getSimpleName}")

      // Verify the union contains both Int and String types (order may vary)
      val unionTypes = listType.ty.asInstanceOf[Union].xs

      // Check that the union has exactly 2 types
      assertEquals(unionTypes.length, 2, s"Expected 2 types in the union but got ${unionTypes.length}")

      // Check that the union contains both IntType and StringType, regardless of order
      val hasIntType = unionTypes.exists(_.isInstanceOf[IntType])
      val hasStringType = unionTypes.exists(_.isInstanceOf[StringType])

      assert(hasIntType, "Union type should contain IntType")
      assert(hasStringType, "Union type should contain StringType")
    }
  }

  test("empty list typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the empty list expression
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("empty-list.chester", "[]"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is a ListTerm
      assert(judge.wellTyped.isInstanceOf[ListTerm], s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // Get the list terms and verify it's empty
      val listTerm = judge.wellTyped.asInstanceOf[ListTerm]
      assertEquals(listTerm.terms.size, 0, "List should have 0 elements")

      // Check the list type structure
      assert(judge.ty.isInstanceOf[ListType], s"Expected ListType but got ${judge.ty.getClass.getSimpleName}")

      val listType = judge.ty.asInstanceOf[ListType]

      // Verify that the element type is NothingType
      assert(listType.ty.isInstanceOf[NothingType], s"Expected NothingType for empty list element type but got ${listType.ty.getClass.getSimpleName}")
    }
  }

  test("nested list typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the nested list expression [[1], [2]]
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("nested-list.chester", "[[1], [2]]"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is a ListTerm
      assert(judge.wellTyped.isInstanceOf[ListTerm], s"Expected ListTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // Get the list terms and verify they are also ListTerms
      val outerListTerm = judge.wellTyped.asInstanceOf[ListTerm]
      assertEquals(outerListTerm.terms.size, 2, "Outer list should have 2 elements")

      // First element should be a ListTerm
      assert(
        outerListTerm.terms(0).isInstanceOf[ListTerm],
        s"First element should be ListTerm but got ${outerListTerm.terms(0).getClass.getSimpleName}"
      )

      // Second element should be a ListTerm
      assert(
        outerListTerm.terms(1).isInstanceOf[ListTerm],
        s"Second element should be ListTerm but got ${outerListTerm.terms(1).getClass.getSimpleName}"
      )

      // Verify the inner lists contain IntTerm elements
      val firstInnerList = outerListTerm.terms(0).asInstanceOf[ListTerm]
      val secondInnerList = outerListTerm.terms(1).asInstanceOf[ListTerm]

      assert(
        firstInnerList.terms(0).isInstanceOf[IntTerm],
        s"Inner list element should be IntTerm but got ${firstInnerList.terms(0).getClass.getSimpleName}"
      )

      assert(
        secondInnerList.terms(0).isInstanceOf[IntTerm],
        s"Inner list element should be IntTerm but got ${secondInnerList.terms(0).getClass.getSimpleName}"
      )

      // Check the list type structure for List(List(Int))
      assert(judge.ty.isInstanceOf[ListType], s"Expected outer ListType but got ${judge.ty.getClass.getSimpleName}")

      val outerListType = judge.ty.asInstanceOf[ListType]

      // Verify that the element type is a ListType
      assert(outerListType.ty.isInstanceOf[ListType], s"Expected ListType but got ${outerListType.ty.getClass.getSimpleName}")

      // Verify that the inner list element type is IntType
      val innerListType = outerListType.ty.asInstanceOf[ListType]
      assert(innerListType.ty.isInstanceOf[IntType], s"Expected IntType for innermost element but got ${innerListType.ty.getClass.getSimpleName}")
    }
  }

  test("block with let binding typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the block expression with let binding
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("block-let.chester", "{let a=0; 1}"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is a BlockTerm
      assert(judge.wellTyped.isInstanceOf[BlockTerm], s"Expected BlockTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // The block should contain a let binding and an integer expression
      val blockTerm = judge.wellTyped.asInstanceOf[BlockTerm]

      // First statement should be a let binding
      assert(blockTerm.statements.head.isInstanceOf[LetStmtTerm], s"Expected LetStmtTerm but got ${blockTerm.statements.head.getClass.getSimpleName}")

      // The return expression should be an IntTerm
      assert(blockTerm.result.isInstanceOf[IntTerm], s"Expected IntTerm but got ${blockTerm.result.getClass.getSimpleName}")

      // Check that the type of the whole expression is IntType (from the final '1')
      assert(judge.ty.isInstanceOf[IntType], s"Expected IntType but got ${judge.ty.getClass.getSimpleName}")
    }
  }

  test("block with let binding returning the bound variable") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the block expression {let x=0; x}
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("block-let-var.chester", "{let x=0; x}"))
      )
        .fold(
          error => fail(s"Failed to parse expression: $error"),
          identity
        )

      // Create reporter and ElabOps for typechecking
      val reporter = new VectorReporter[TyckProblem]()
      val elabOps = ElabOps(reporter, NoopSemanticCollector)

      // Infer the type using the DefaultElaborator
      val judge = DefaultElaborator.inferPure(expr)(using elabOps)

      // Assert that there are no errors
      assertEquals(reporter.getReports.isEmpty, true, s"Expected no type errors, but got: ${reporter.getReports}")

      // Check that the elaborated term is a BlockTerm
      assert(judge.wellTyped.isInstanceOf[BlockTerm], s"Expected BlockTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // The block should contain a let binding and a variable reference
      val blockTerm = judge.wellTyped.asInstanceOf[BlockTerm]

      // First statement should be a let binding
      assert(blockTerm.statements.head.isInstanceOf[LetStmtTerm], s"Expected LetStmtTerm but got ${blockTerm.statements.head.getClass.getSimpleName}")

      // Verify the let binding details
      val letStmt = blockTerm.statements.head.asInstanceOf[LetStmtTerm]
      assertEquals(letStmt.localv.name, "x", "Variable name should be 'x'")
      assert(letStmt.value.isInstanceOf[IntTerm], s"Let binding value should be IntTerm but got ${letStmt.value.getClass.getSimpleName}")

      // The return expression should be a LocalV (variable reference)
      assert(blockTerm.result.isInstanceOf[LocalVar], s"Expected LocalV but got ${blockTerm.result.getClass.getSimpleName}")

      // Verify it refers to the correct variable
      val varRef = blockTerm.result.asInstanceOf[LocalVar]
      assertEquals(varRef.name, "x", "Variable reference should be to 'x'")

      // Check that the type of the whole expression is IntType (inferred from x which has type Int)
      assert(judge.ty.isInstanceOf[IntType], s"Expected IntType but got ${judge.ty.getClass.getSimpleName}")
    }
  }
}
