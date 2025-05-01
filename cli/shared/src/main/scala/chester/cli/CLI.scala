package chester.cli

import chester.core.parseCheckTAST
import chester.error.*
import chester.error.Problem.Severity
import chester.integrity.IntegrityCheck
import chester.reader.{FilePath, FilePathImpl}
import chester.repl.REPLEngine
import chester.tyck.{TyckResult, Tycker}
import chester.utils.env.Environment
import chester.utils.io.*
import chester.utils.term.{Terminal, TerminalInit}
import chester.syntax.TASTPackage.{LoadedModules, TAST}
import chester.utils.doc.*
import chester.BuildInfo
import chester.cli.Config.*
import chester.syntax.concrete.Expr
import upickle.default.{read, readBinary, write, writeBinary}
import cats.implicits.*
import chester.i18n.*
import chester.readerv2.ChesterReaderV2

import scala.language.experimental.betterFors

object CLI {
  def spawn[F[_]](config: Option[Config])(using
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
  def run(configOpt: Option[Config]): F[Unit] =
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

  private def noop(): F[Unit] =
    Runner.pure(())

  private def spawnREPLEngine(): F[Unit] =
    Terminal.runTerminal(TerminalInit.Default) {
      REPLEngine[F]
    }

  private def runFileOrDirectory(fileOrDir: String): F[Unit] = for {
    _ <- IO.println(t"Expect one file for type checking (more support will be added later) $fileOrDir...")
    _ <- ChesterReaderV2
      .parseTopLevel(FilePath(fileOrDir))
      .fold(
        _ => ???,
        { parsedBlock =>
          assert(read[Expr](write[Expr](parsedBlock)) == parsedBlock)
          assert(readBinary[Expr](writeBinary[Expr](parsedBlock)) == parsedBlock)
          val tyckResult = Tycker.check(parsedBlock)
          if (tyckResult.errorsEmpty) {
            // This is equivalent to TyckResult.Success case
            val result = tyckResult.result
            if (result.collectMeta.nonEmpty) {
              ???
            }
            val text = StringPrinter.render(result)(using PrettierOptions.Default)
            IO.println(text)
          } else {
            // This is equivalent to TyckResult.Failure case
            val errors = tyckResult.problems.collect { case e: TyckError => e }
            val warnings = tyckResult.problems.collect { case w: TyckWarning => w }

            given sourceReader: SourceReader = SourceReader.default
            given prettierOptions: PrettierOptions = PrettierOptions.Default

            for {
              _ <- errors.traverse(error => IO.println(FansiPrettyPrinter.render(error.renderDoc, 80).render))
              _ <- warnings.traverse(warning => IO.println(FansiPrettyPrinter.render(warning.renderDoc, 80).render))
            } yield ()
          }
        }
      )
  } yield ()

  private def runIntegrityCheck(): F[Unit] = for {
    _ <- IO.println("Running integrity check...")
    _ = IntegrityCheck()
    _ <- Runner.pure(())
  } yield ()

  private def compileFiles(inputs: Seq[String], targetDir: String, tastDirs: Seq[String]): F[Unit] =
    inputs.foldLeft(Runner.pure(()))((acc, inputFile) => acc.flatMap(_ => this.compileFile(inputFile, targetDir, tastDirs)))

  private def loadTASTs(tastDirs: Seq[String]): F[LoadedModules] =
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

  private def compileFile(inputFile: String, targetDir: String, tastDirs: Seq[String]): F[Unit] = {
    // Expected input file extension
    val expectedExtension = ".chester"

    if (!inputFile.endsWith(expectedExtension)) {
      IO.println(
        t"Error: Input file '$inputFile' does not have the expected '$expectedExtension' extension."
      )
    } else {
      // Generate output file name by replacing the extension
      val outputFileName = inputFile.stripSuffix(expectedExtension) + ".tast"
      val outputPath = io.pathOps.join(io.pathOps.of(targetDir), outputFileName)

      val source = FilePath(inputFile)

      object reporter extends Reporter[Problem] {
        private var varErrors: Boolean = false

        override def apply(problem: Problem): Unit = {
          given sourceReader: SourceReader = SourceReader.default
          given prettierOptions: PrettierOptions = PrettierOptions.Default

          problem.severity match {
            case Severity.Error =>
              varErrors = true
              println(FansiPrettyPrinter.render(problem.renderDoc, 80).render)
            case Severity.Warning =>
              println(FansiPrettyPrinter.render(problem.renderDoc, 80).render)
            case _ =>
              println(FansiPrettyPrinter.render(problem.renderDoc, 80).render)
          }
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
            IO.println(t"Compilation failed for $inputFile with errors.")
          } else {
            for {
              _ <- IO.createDirRecursiveIfNotExists(io.pathOps.of(targetDir))
              _ <- IO.write(outputPath, upickle.default.writeBinary(tast))
              _ <- IO.println(t"Compiled $inputFile to $outputPath")
            } yield ()
          }
      } yield ()
    }
  }

  private def decompileFile(inputFile: String): F[Unit] =
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
          IO.println(t"Error: File $inputFile does not exist.")
        }
    } yield ()

  private def content(name: String): String =
    t"""{
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
  private def initializePackageJson(): F[Unit] =
    for {
      currentDir <- IO.workingDir
      packageJsonPath = io.pathOps.join(currentDir, "package.json")
      _ <- IO.writeString(packageJsonPath, content(io.pathOps.baseName(currentDir)))
      _ <- IO.println("Initialized package.json in the current directory.")
    } yield ()

  private def installDependencies(): F[Unit] = for {
    _ <- IO.call(Vector("pnpm", "install"))
  } yield ()

  private def addPackages(packages: Seq[String]): F[Unit] = for {
    _ <- IO.call(Vector("pnpm", "add") ++ packages.map(p => t"$p.chester"))
  } yield ()

  private def selfUpdate(): F[Unit] = for {
    _ <- IO.call(Vector("proto", "install", "chester"))
  } yield ()

  private def formatFiles(files: Seq[String]): F[Unit] =
    for {
      _ <- IO.println(t"Formatting files: ${files.mkString(", ")}")
      _ <- IO.println("WIP")
      _ <- Runner.pure(())
    } yield ()
}
