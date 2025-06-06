package chester.cli

import chester.backend.ts.TSBackend
import chester.error.reporterToEither
import chester.elab.*
import chester.error.{TyckProblem, VectorReporter}
import chester.integrity.IntegrityCheck
import chester.repl.REPLEngine
import chester.reader.{FileNameAndContent, FilePathImpl}
import chester.readerv1.ChesterReaderV1
import chester.elab.Context
import chester.elab.api.NoopSemanticCollector
import chester.utils.elab.*
import chester.utils.env.Environment
import chester.utils.io.*
import chester.utils.term.{Terminal, TerminalInit}

import scala.language.experimental.betterFors

object CLI {
  def run[F[_]](config: Config)(using
      runner: Runner[F],
      terminal: Terminal[F],
      env: Environment,
      path: FilePathImpl,
      spawn: Spawn[F],
      io: IO[F]
  ): Unit =
    Spawn.spawn {
      (new CLI[F]).run(config)
    }
}

class CLI[F[_]](using
    runner: Runner[F],
    terminal: Terminal[F],
    env: Environment,
    path: FilePathImpl,
    io: IO[F]
) {
  private def spawnREPLEngine(): F[Unit] =
    Terminal.runTerminal(TerminalInit.Default) {
      REPLEngine[F]
    }
  def run(config: Config): F[Unit] = config match {
    case Config.Version =>
      IO.println(s"Chester version: ${chester.BuildInfo.version}")
    case Config.Integrity =>
      IntegrityCheck()
      IO.println("Integrity check passed.")
    case Config.Run(None)    => spawnREPLEngine()
    case Config.Run(Some(_)) => ???
    case Config.Compile(target0, inputFile, outputFile0) =>
      val target = target0.getOrElse("typescript")
      val outputFile = outputFile0.getOrElse("a.out")
      target match {
        case "typescript" =>
          for {
            inputPath = io.pathOps.of(inputFile)
            inputExists <- IO.exists(inputPath)
            _ <-
              if (!inputExists) {
                IO.println(s"Input file '$inputFile' does not exist.", toStderr = true)
              } else {
                for {
                  inputContent <- IO.readString(inputPath)
                  parsed = reporterToEither(ChesterReaderV1.parseTopLevel(FileNameAndContent(inputFile, inputContent)))
                  _ <- parsed match {
                    case Left(error) =>
                      IO.println(s"Error parsing input file '$inputFile': $error")
                    case Right(ast) =>
                      platformInfo.withValue(TypescriptPlatformInfo) {
                        import chester.elab.Defaults.given
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
                        if (errors.nonEmpty) {
                          IO.println(s"Errors found during elaboration: ${errors.mkString("\n")}", toStderr = true)
                        } else {
                          val tast1 = tast.zonkAll
                          val outputPath = io.pathOps.of(outputFile)
                          val compiled = TSBackend.compileModule(tast1)
                          for {
                            _ <- IO.println(s"Writing output to '$outputFile'.")
                            _ <- IO.writeString(outputPath, compiled.toString, writeMode = WriteMode.Overwrite)
                          } yield ()
                        }
                      }
                  }
                } yield ()
              }
          } yield ()
        case unsupported =>
          IO.println(s"Unsupported target: $unsupported.", toStderr = true)
      }
  }
}
