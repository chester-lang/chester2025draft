package chester.cli

import chester.core.parseCheckTAST
import chester.error.Problem
import chester.error.Problem.Severity
import chester.integrity.IntegrityCheck
import chester.reader.{FilePath, FilePathImpl}
import chester.repl.REPLEngine
import chester.tyck.Reporter
import chester.utils.env.Environment
import chester.utils.io.*
import chester.utils.term.{Terminal, TerminalInit}
import chester.syntax.TASTPackage.{LoadedModules, TAST}
import chester.utils.doc.*
import chester.BuildInfo
import chester.cli.Config.*

object CLI {
  def spawn[F[_]](config: Option[Config])(using
      runner: Runner[F],
      terminal: Terminal[F],
      env: Environment,
      path: FilePathImpl,
      spawn: Spawn[F],
      io: IO[F]
  ): Unit = {
    Spawn.spawn {
      (new CLI[F]).run(config)
    }
  }
}

class CLI[F[_]](using
    runner: Runner[F],
    terminal: Terminal[F],
    env: Environment,
    path: FilePathImpl,
    io: IO[F]
) {
  def run(configOpt: Option[Config]): F[Unit] = {
    configOpt match {
      case Some(config) =>
        config match {
          case RunConfig(inputOpt) =>
            inputOpt match {
              case None            => this.spawnREPLEngine()
              case Some("-")       => this.spawnREPLEngine()
              case Some(fileOrDir) => this.runFileOrDirectory(fileOrDir)
            }
          case IntegrityConfig =>
            this.runIntegrityCheck()
          case CompileConfig(inputs, targetDir, tastDirs) =>
            this.compileFiles(inputs, targetDir, tastDirs)
          case DecompileConfig(inputFile) =>
            this.decompileFile(inputFile)
          case InitConfig =>
            this.initializePackageJson()
          case InstallConfig =>
            this.installDependencies()
          case AddConfig(packages) =>
            this.addPackages(packages)
          case SelfUpdateConfig =>
            this.selfUpdate()
          case FormatConfig(files) => // Added case
            this.formatFiles(files)
        }
      case None =>
        // Arguments are bad, error message will have been displayed
        this.noop()
    }
  }

  def noop(): F[Unit] = {
    Runner.pure(())
  }

  def spawnREPLEngine(): F[Unit] = {
    Terminal.runTerminal(TerminalInit.Default) {
      REPLEngine[F]
    }
  }

  // Evaluate from file or directory
  def runFileOrDirectory(fileOrDir: String): F[Unit] = for {
    _ <- IO.println(s"Running from $fileOrDir...")
    // Implement your logic here
    _ <- Runner.pure(())
  } yield ()

  def runIntegrityCheck(): F[Unit] = for {
    _ <- IO.println("Running integrity check...")
    _ = IntegrityCheck()
    _ <- Runner.pure(())
  } yield ()

  def compileFiles(inputs: Seq[String], targetDir: String, tastDirs: Seq[String]): F[Unit] = {
    inputs.foldLeft(Runner.pure(())) { (acc, inputFile) =>
      acc.flatMap(_ => this.compileFile(inputFile, targetDir, tastDirs))
    }
  }

  def loadTASTs(tastDirs: Seq[String]): F[LoadedModules] = {
    tastDirs.foldLeft(Runner.pure(LoadedModules.Empty)) { (acc, dir) =>
      for {
        modules <- acc
        path = io.pathOps.of(dir)
        isDir <- IO.isDirectory(path)
        newModules <-
          if (isDir) {
            for {
              files <- IO.listFiles(path)
              tastFiles = files.filter(f => io.pathOps.asString(f).endsWith(".tast"))
              modulesWithFiles <- tastFiles.foldLeft(Runner.pure(modules)) { (modAcc, file) =>
                for {
                  mods <- modAcc
                  bytes <- IO.read(file)
                  tast <- Runner.pure(upickle.default.readBinary[TAST](bytes))
                } yield mods.add(tast)
              }
            } yield modulesWithFiles
          } else {
            Runner.pure(modules)
          }
      } yield newModules
    }
  }

  def compileFile(inputFile: String, targetDir: String, tastDirs: Seq[String]): F[Unit] = {
    // Expected input file extension
    val expectedExtension = ".chester"

    if (!inputFile.endsWith(expectedExtension)) {
      IO.println(
        s"Error: Input file '$inputFile' does not have the expected '$expectedExtension' extension."
      )
    } else {
      // Generate output file name by replacing the extension
      val outputFileName = inputFile.stripSuffix(expectedExtension) + ".tast"
      val outputPath = io.pathOps.join(io.pathOps.of(targetDir), outputFileName)

      val source = FilePath(inputFile)

      object reporter extends Reporter[Problem] {
        private var varErrors: Boolean = false

        override def apply(problem: Problem): Unit = problem.severity match {
          case Severity.Error =>
            varErrors = true
            println(s"Error: $problem")
          case Severity.Warning =>
            println(s"Warning: $problem")
          case _ =>
            println(s"Info: $problem")
        }

        def hasErrors: Boolean = varErrors
      }
      given reportG: reporter.type = reporter

      for {
        // Load TASTs from the specified directories
        loadedModules <- loadTASTs(tastDirs)
        tast = parseCheckTAST(source, loadedModules = loadedModules)

        _ <-
          if (reporter.hasErrors) {
            IO.println(s"Compilation failed for $inputFile with errors.")
          } else {
            for {
              _ <- IO.createDirRecursiveIfNotExists(io.pathOps.of(targetDir))
              _ <- IO.write(outputPath, upickle.default.writeBinary(tast))
              _ <- IO.println(s"Compiled $inputFile to $outputPath")
            } yield ()
          }
      } yield ()
    }
  }

  def decompileFile(inputFile: String): F[Unit] = {
    for {
      inputPath = stringToPath(inputFile)
      fileExists <- IO.exists(inputPath)
      _ <-
        if (fileExists) {
          for {
            bytes <- IO.read(inputPath)
            tast <- Runner.pure(upickle.default.readBinary[TAST](bytes))
            // Use `toDoc` and `FansiPrettyPrinter` to render the TAST
            doc = tast.ast.toDoc(using PrettierOptions.Default)
            rendered = FansiPrettyPrinter.render(doc, maxWidth = 80).render
            _ <- IO.println(rendered)
          } yield ()
        } else {
          IO.println(s"Error: File $inputFile does not exist.")
        }
    } yield ()
  }

  private def content(name: String): String =
    s"""{
       |  "name": "$name.chester",
       |  "version": "0.1.0",
       |  "description": "A Chester library",
       |  "files": [
       |    "src"
       |  ],
       |  "scripts": {
       |    "test": "echo \\\"Error: no test specified\\\" && exit 1"
       |  },
       |  "engines": {
       |    "chester": "${BuildInfo.version}"
       |  },
       |  "keywords": [],
       |  "author": "",
       |  "license": ""
       |}""".stripMargin
  def initializePackageJson(): F[Unit] = {
    for {
      currentDir <- IO.workingDir
      packageJsonPath = io.pathOps.join(currentDir, "package.json")
      _ <- IO.writeString(packageJsonPath, content(io.pathOps.baseName(currentDir)))
      _ <- IO.println("Initialized package.json in the current directory.")
    } yield ()
  }

  def installDependencies(): F[Unit] = for {
    _ <- IO.call(Vector("pnpm", "install"))
  } yield ()

  def addPackages(packages: Seq[String]): F[Unit] = for {
    _ <- IO.call(Vector("pnpm", "add") ++ packages.map(p => s"${p}.chester"))
  } yield ()

  def selfUpdate(): F[Unit] = for {
    _ <- IO.call(Vector("proto", "install", "chester"))
  } yield ()

  def formatFiles(files: Seq[String]): F[Unit] = {
    for {
      _ <- IO.println(s"Formatting files: ${files.mkString(", ")}")
      _ <- IO.println("WIP")
      _ <- Runner.pure(())
    } yield ()
  }
}
