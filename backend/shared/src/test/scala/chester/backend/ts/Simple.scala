package chester.backend.ts

import chester.elab.*
import chester.reader.FileNameAndContent
import chester.readerv1.ChesterReaderV1
import munit.FunSuite
import chester.error.{TyckProblem, VectorReporter, reporterToEither}
import chester.tyck.Context
import chester.tyck.api.NoopSemanticCollector
import chester.utils.elab.{HandlerConf, SolverFactory, SolverOps}
import chester.elab.Defaults.given

class Simple extends FunSuite {
  test("compile simple assignment") {
    platformInfo.withValue(TypescriptPlatformInfo) {
      val input = "let x = 42;"
      val inputFile = "test.chester"
      val parsed = reporterToEither(ChesterReaderV1.parseTopLevel(FileNameAndContent(inputFile, input)))
      assert(parsed.isRight, s"Parsing failed: ${parsed.left.getOrElse("Unknown error")}")
      val ast = parsed.toOption.get
      val reporter = new VectorReporter[TyckProblem]()

      given elabOps: ElabOps = ElabOps(reporter, NoopSemanticCollector)

      given solver: SolverOps = summon[SolverFactory](summon[HandlerConf[ElabOps]])

      given Context = Context.default

      val elab = summon[Elab]
      val tast = elab.checkWholeUnit(inputFile, ast)
      solver.run()
      assume(solver.stable, "Solver did not stabilize after elaboration.")
      val problems = reporter.getReports
      val errors = problems.filter(_.isError)
      assert(errors.isEmpty, s"Errors found during elaboration: ${errors.mkString(", ")}")
      val tast1 = tast.zonkAll
      val compiled = TSBackend.compileModule(tast1)
      assert(compiled.stmts.length == 1, "Expected exactly one statement in the compiled output.")
      assertEquals(compiled.stmts.head.toString, "const x: number = 42;")
    }
  }

}
