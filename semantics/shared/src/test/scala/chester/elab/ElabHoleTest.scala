package chester.elab

import chester.error.{TyckProblem, VectorReporter}
import chester.elab.{DefaultElaborator, ElabOps, TypescriptPlatformInfo, platformInfo}
import chester.reader.FileNameAndContent
import chester.readerv1.ChesterReaderV1
import chester.syntax.core.*
import chester.elab.api.NoopSemanticCollector
import munit.FunSuite
import chester.error.reporterToEither

class ElabHoleTest extends FunSuite {
  test("?hole should produce HoleTerm") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      // Parse the ?hole expression
      val expr = reporterToEither(
        ChesterReaderV1
          .parseExpr(FileNameAndContent("hole.chester", "?hole"))
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

      // Check that the elaborated term is a HoleTerm - this is the main test
      assert(judge.wellTyped.isInstanceOf[HoleTerm], s"Expected HoleTerm but got ${judge.wellTyped.getClass.getSimpleName}")
    }
  }
}