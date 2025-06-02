package chester.cliv2

import caseapp.*

import chester.reader.given
import chester.utils.env.DefaultEnv
import chester.utils.io.given
import chester.utils.io.impl.given

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
      printLine("Usage: chester run [file]", toStderr = true)
      exit(1)
    }
}

object CommandVersion extends Command[NoOptions] {
  override def names: List[List[String]] = List(
    List("version")
  )
  def run(options: NoOptions, args: RemainingArgs): Unit = {
    CLI.spawn(Config.Version)
  }
}

object Main extends CommandsEntryPoint {
  def progName = "chester"
  def commands: Seq[Command[?]] = Seq(
    CommandRun
  )
  override def defaultCommand: Option[Command[?]] = Some(CommandRun)
  override def enableCompleteCommand = true
  override def enableCompletionsCommand = true
}
