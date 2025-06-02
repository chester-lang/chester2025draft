package chester.cliv2

import chester.repl.REPLEngine

import scala.language.experimental.betterFors

import chester.reader.FilePathImpl
import chester.utils.env.Environment
import chester.utils.io.{IO, Runner, Spawn}
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
    case Config.Compile(target0, inputFile, outputFile0) => {
      val target = target0.getOrElse("typescript")
      val outputFile = outputFile0.getOrElse("a.out")
      ???
    }
  }
}
