package chester.cliv2

import chester.elab.{Elab, ElabOps}
import chester.error.{TyckProblem, VectorReporter}
import chester.repl.REPLEngine

import scala.language.experimental.betterFors
import chester.reader.{FileNameAndContent, FilePathImpl}
import chester.readerv2.ChesterReaderV2
import chester.tyck.Context
import chester.tyck.api.NoopSemanticCollector
import chester.utils.elab.*
import chester.utils.env.Environment
import chester.utils.io.*
import chester.utils.term.{Terminal, TerminalInit}

object CLI {
  def spawn[F[_]](config: Config)(using
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
    case Config.Run(None)            => spawnREPLEngine()
    case Config.Run(Some(fileOrDir)) => ???
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
                IO.println(s"Input file '$inputFile' does not exist.")
              } else {
                for {
                  inputContent <- IO.readString(inputPath)
                  parsed = ChesterReaderV2.parseTopLevel(FileNameAndContent(inputFile, inputContent))
                  _ <- parsed match {
                    case Left(error) =>
                      IO.println(s"Error parsing input file '$inputFile': $error")
                    case Right(ast) =>
                      import chester.elab.Defaults.given
                      val reporter = new VectorReporter[TyckProblem]()
                      given elabOps: ElabOps = ElabOps(reporter, NoopSemanticCollector)
                      given solver: SolverOps = summon[SolverFactory](summon[HandlerConf[ElabOps]])
                      given Context = Context.default
                      val elab = summon[Elab]
                      ???
                  }
                } yield ()
              }
          } yield ()
        case unsupported =>
          IO.println(s"Unsupported target: $unsupported.")
      }
  }
}
