package chester.cliv2

import caseapp.*
import caseapp.core.Error
import chester.reader.given
import chester.utils.env.DefaultEnv
import chester.utils.io.given
import chester.utils.io.impl.given
import chester.i18n.*

case class NoOptions()

object CommandRun extends Command[NoOptions] {
  override def names: List[List[String]] = List(
    List("run")
  )
  def run(options: NoOptions, args: RemainingArgs): Unit =
    if (args.all.isEmpty) {
      CLI.spawn(Config.Run(None))
    } else if (args.all.size == 1) {
      CLI.spawn(Config.Run(Some(args.all.head)))
    } else {
      printLine(t"run subcommand only accept at most one argument at this moment", toStderr = true)
      exit(1)
    }
}

object CommandVersion extends Command[NoOptions] {
  override def names: List[List[String]] = List(
    List("version")
  )
  def run(options: NoOptions, args: RemainingArgs): Unit =
    CLI.spawn(Config.Version)
}

case class CompileOptions(target: Option[String] = None, output: Option[String] = None)

object CommandCompile extends Command[CompileOptions] {
  override def names: List[List[String]] = List(
    List("compile")
  )
  def run(options: CompileOptions, args: RemainingArgs): Unit =
    if (args.all.size != 1) {
      printLine(t"compile subcommand requires exactly one argument", toStderr = true)
      exit(1)
    } else {
      val inputFile = args.all.head
      val outputFile = options.output
      CLI.spawn(Config.Compile(options.target, inputFile, outputFile))
    }
}

object CommandHelp extends Command[NoOptions] {
  override def names: List[List[String]] = List(
    List("help")
  )
  def run(options: NoOptions, args: RemainingArgs): Unit =
    Main.printUsage()
}

object MainCommand extends Command[NoOptions] {
  override def names: List[List[String]] = List(
    List(Main.progName)
  )
  override def helpAsked(progName: String, maybeOptions: Either[Error, NoOptions]): Nothing = {
    var help = Main.help
    if (progName.nonEmpty) help = help.copy(progName = progName)
    val usage = Main.help.help(Main.helpFormat, showHidden = false)
    printLine(usage, toStderr = false)
    exit(0)
  }
  override def usageAsked(progName: String, maybeOptions: Either[Error, NoOptions]): Nothing = {
    var help = Main.help
    if (progName.nonEmpty) help = help.copy(progName = progName)
    val usage = Main.help.help(Main.helpFormat, showHidden = false)
    printLine(usage, toStderr = false)
    exit(0)
  }
  def run(options: NoOptions, args: RemainingArgs): Unit =
    if (args.all.isEmpty) {
      CLI.spawn(Config.Run(None))
    } else {
      printLine(t"Invalid command.", toStderr = true)
      val usage = Main.help.help(helpFormat, showHidden = false)
      printLine(usage, toStderr = true)
      exit(1)
    }
}

object Main extends CommandsEntryPoint {
  override def progName = "chester"
  override def commands: Seq[Command[?]] = Seq(
    CommandRun,
    CommandVersion,
    CommandHelp,
    CommandCompile
  )
  override def description: String = t"Chester CLI - A command line interface for Chester"
  override def summaryDesc: String = t"Chester CLI is a command line interface for Chester, a tool for managing and running tasks."
  override def defaultCommand: Option[Command[?]] = Some(MainCommand)
  override def enableCompleteCommand = true
  override def enableCompletionsCommand = true
}
