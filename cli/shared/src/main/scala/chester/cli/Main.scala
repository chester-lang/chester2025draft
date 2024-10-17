package chester.cli

import chester.parser.{given}
import chester.BuildInfo
import chester.utils.fileExists
import scopt.OParser
import chester.utils.env.DefaultEnv
import chester.utils.io.*
import chester.utils.io.impl.*

// Import the generated BuildInfo object

object Main {

  sealed trait Config

  case class RunConfig(input: Option[String]) extends Config

  case object IntegrityConfig extends Config

  case class CompileConfig(
      inputs: Seq[String],
      targetDir: String = "."
  ) extends Config

  case class DecompileConfig(input: String) extends Config

  case object InitConfig extends Config

  // Add the new InstallConfig
  case object InstallConfig extends Config

  // Parsing state class with default command set to "run"
  case class CliConfig(
      command: String = "run", // Default command is "run"
      input: Option[String] = None,
      inputs: Seq[String] = Seq(),
      targetDir: String = ".",
      version: Boolean = false // Add a flag for the version option
  )

  def main(args: Array[String]): Unit = {
    if (false) platform.testLoadingJS()

    val builder = OParser.builder[CliConfig]
    val parser = {
      import builder._
      OParser.sequence(
        programName("chester"),
        head("chester", BuildInfo.version), // Use BuildInfo.version here

        // Global version option
        opt[Unit]('v', "version")
          .action((_, c) => c.copy(version = true))
          .text("Print version information and exit"),

        // Command for "run"
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

        // Command for "integrity"
        cmd("integrity")
          .action((_, c) => c.copy(command = "integrity"))
          .text("Run integrity check"),

        // Command for "compile"
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
              )
          ),

        // Command for "decompile"
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

        // Command for "init"
        cmd("init")
          .action((_, c) => c.copy(command = "init"))
          .text("Initialize a Chester project in the current directory"),

        // Command for "install" (aliased as "i")
        cmd("install")
          .abbr("i") // Alias the command as "i"
          .action((_, c) => c.copy(command = "install"))
          .text("Install dependencies"),

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
              CompileConfig(cliConfig.inputs, cliConfig.targetDir)
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
          case "genSemanticDB" =>
            platform.genSemanticDB(cliConfig)
            return
          case _ =>
            println("Invalid command")
            return
        }
        Program.spawn(Some(config))
      case None =>
        // If parsing fails, default to "run" command when no args are provided
        if (args.isEmpty) {
          Program.spawn(Some(RunConfig(None)))
        } else {
          // Arguments are bad, error message will have been displayed
          // Program.spawn(None)
        }
    }
  }
}
