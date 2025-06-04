package chester.elab

import chester.error.{TyckProblem, VectorReporter}
import chester.elab.{DefaultElaborator, ElabOps, TypescriptPlatformInfo, platformInfo}
import chester.reader.FileNameAndContent
import chester.readerv1.ChesterReaderV1
import chester.syntax.core.*
import chester.elab.api.NoopSemanticCollector
import munit.FunSuite
import chester.error.reporterToEither

class ElabObjectTest extends FunSuite {
  test("empty object %{} typechecking with new elab") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the empty object expression %{}
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("empty-object.chester", "%{}"))
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

      // Check that the elaborated term is an ObjectTerm
      assert(judge.wellTyped.isInstanceOf[ObjectTerm], s"Expected ObjectTerm but got ${judge.wellTyped.getClass.getSimpleName}")

      // Get the object terms and verify it's empty
      val objectTerm = judge.wellTyped.asInstanceOf[ObjectTerm]
      assertEquals(objectTerm.clauses.size, 0, "Object should have 0 clauses")

      // Check the object type structure  
      assert(judge.ty.isInstanceOf[ObjectType], s"Expected ObjectType but got ${judge.ty.getClass.getSimpleName}")

      val objectType = judge.ty.asInstanceOf[ObjectType]
      
      // Verify that the object type has no clauses (empty object)
      assertEquals(objectType.fieldTypes.size, 0, "ObjectType should have 0 fieldTypes for empty object")
    }
  }
}