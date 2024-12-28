package chester.cli

import chester.reader.given
import chester.BuildInfo
import chester.utils.fileExists
import scopt.OParser
import chester.utils.env.DefaultEnv
import chester.utils.io.*
import chester.utils.io.impl.*
import chester.cli.Config.*

object Main {

  // Parsing state class with default command set to "run"
  case class CliConfig(
      command: String = "run", // Default command is "run"
      input: Option[String] = None,
      inputs: Seq[String] = Seq(),
      targetDir: String = ".",
      version: Boolean = false,
      packages: Seq[String] = Seq(),
      tastDirs: Seq[String] = Seq(),
      filesToFormat: Seq[String] = Seq()
  )

  def main(args: Array[String]): Unit = {
    chester.cli.platform.testFunctionalities()

    val builder = OParser.builder[CliConfig]
    val parser = {
      import builder._
      OParser.sequence(
        programName("chester"),
        head("chester", BuildInfo.version),
        opt[Unit]('v', "version")
          .action((_, c) => c.copy(version = true))
          .text("Print version information and exit"),
        cmd("run")
          .action((_, c) => c.copy(command = "run"))
          .text("Run expressions")
          .children(
            arg[String]("input")
              .optional()
              .validate {
                case "-"                      => success
                case path if fileExists(path) => success
                case path =>
                  failure(
                    s"Invalid input. Provide '-' for stdin, or a valid file/directory. Provided: $path"
                  )
              }
              .action((x, c) => c.copy(input = Some(x)))
              .text("Input file or directory. Use '-' for stdin.")
          ),
        cmd("integrity")
          .action((_, c) => c.copy(command = "integrity"))
          .text("Run integrity check"),
        cmd("compile")
          .action((_, c) => c.copy(command = "compile"))
          .text("Compile Chester source files")
          .children(
            arg[String]("inputs...")
              .unbounded()
              .required()
              .validate {
                case path if fileExists(path) => success
                case path =>
                  failure(
                    s"Invalid input. Provide a valid file. Provided: $path"
                  )
              }
              .action((x, c) => c.copy(inputs = c.inputs :+ x))
              .text("Input source files."),
            opt[String]("target-dir")
              .abbr("d")
              .optional()
              .action((x, c) => c.copy(targetDir = x))
              .text(
                "Target directory for compiled outputs (defaults to current directory)."
              ),
            opt[Seq[String]]("tast-dir")
              .abbr("t")
              .optional()
              .valueName("<dir1>,<dir2>...")
              .action((x, c) => c.copy(tastDirs = x))
              .text("Directories containing TAST files to load")
          ),
        cmd("decompile")
          .action((_, c) => c.copy(command = "decompile"))
          .text("Decompile a .tast binary file")
          .children(
            arg[String]("input")
              .required()
              .validate {
                case path if fileExists(path) => success
                case path =>
                  failure(
                    s"Invalid input. Provide a valid .tast file. Provided: $path"
                  )
              }
              .action((x, c) => c.copy(input = Some(x)))
              .text("Input .tast binary file.")
          ),

        // Command for "genSemanticDB"
        cmd("genSemanticDB")
          .action((_, c) => c.copy(command = "genSemanticDB"))
          .text("Generate SemanticDB for testing")
          .children(
            arg[String]("input")
              .required()
              .validate {
                case path if fileExists(path) => success
                case path =>
                  failure(
                    s"Invalid input. Provide a valid source file or directory. Provided: $path"
                  )
              }
              .action((x, c) => c.copy(input = Some(x)))
              .text("Input source file or directory.")
          ),
        cmd("init")
          .action((_, c) => c.copy(command = "init"))
          .text("Initialize a Chester project in the current directory"),
        cmd("add")
          .action((_, c) => c.copy(command = "add"))
          .text("Add one or more packages")
          .children(
            arg[String]("<packages>...")
              .unbounded()
              .required()
              .action((x, c) => c.copy(packages = c.packages :+ x))
              .text("Package names to add")
          ),
        cmd("install")
          .abbr("i")
          .action((_, c) => c.copy(command = "install"))
          .text("Install dependencies"),
        cmd("self-update")
          .action((_, c) => c.copy(command = "self-update"))
          .text("Update Chester CLI to the latest version"),
        cmd("format") // Added command
          .action((_, c) => c.copy(command = "format"))
          .text("Format Chester source files")
          .children(
            arg[String]("files...")
              .unbounded()
              .required()
              .validate {
                case path if fileExists(path) => success
                case path =>
                  failure(s"Invalid input. Provide valid file(s). Provided: $path")
              }
              .action((x, c) => c.copy(filesToFormat = c.filesToFormat :+ x))
              .text("Source files to format.")
          ),

        // Handle case where user might omit "run" and just provide input directly
        arg[String]("input")
          .optional()
          .validate {
            case "-"                      => success
            case path if fileExists(path) => success
            case path =>
              failure(
                s"Invalid input. Provide '-' for stdin, or a valid file/directory. Provided: $path"
              )
          }
          .action((x, c) => c.copy(input = Some(x)))
          .hidden()
      )
    }

    // Parse the arguments
    OParser.parse(parser, platform.argsPlatform(args), CliConfig()) match {
      case Some(cliConfig) if cliConfig.version =>
        // Handle version flag
        println(s"Chester version ${BuildInfo.version}")
      case Some(cliConfig) =>
        val config: Config = cliConfig.command match {
          case "run" =>
            RunConfig(cliConfig.input)
          case "integrity" =>
            IntegrityConfig
          case "compile" =>
            if (cliConfig.inputs.nonEmpty) {
              CompileConfig(cliConfig.inputs, cliConfig.targetDir, cliConfig.tastDirs)
            } else {
              println(
                "Error: At least one input file is required for compile command."
              )
              return
            }
          case "decompile" =>
            cliConfig.input match {
              case Some(inputFile) =>
                DecompileConfig(inputFile)
              case None =>
                println("Error: Input file is required for decompile command.")
                return
            }
          case "init" =>
            InitConfig
          case "install" =>
            InstallConfig
          case "add" =>
            if (cliConfig.packages.nonEmpty) {
              AddConfig(cliConfig.packages)
            } else {
              println("Error: At least one package name is required for add command.")
              return
            }
          case "self-update" =>
            SelfUpdateConfig
          case "format" =>
            if (cliConfig.filesToFormat.nonEmpty) {
              FormatConfig(cliConfig.filesToFormat)
            } else {
              println("Error: At least one file is required for format command.")
              return
            }
          case _ =>
            println("Invalid command")
            return
        }
        CLI.spawn(Some(config))
      case None =>
        // If parsing fails, default to "run" command when no args are provided
        if (args.isEmpty) {
          CLI.spawn(Some(RunConfig(None)))
        } else {
          // Arguments are bad, error message will have been displayed
          // CLI.spawn(None)
        }
    }
  }
}
