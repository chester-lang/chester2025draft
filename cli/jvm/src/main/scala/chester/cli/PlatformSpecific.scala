package chester.cli

import chester.cli.Main.CliConfig
import chester.tyck.SemanticDBGenerator
import chester.utils.*

object PlatformSpecific {
  def testLoadingJS(): Unit = {
    println(chester.Js4Jvm.test(chester.Js4Jvm.helloFromJs))
  }
  def genSemanticDB(config: CliConfig): Unit = {
    val inputPath = config.input.getOrElse {
      println("Error: Input path is required.")
      return
    }

    val path = os2.path(inputPath)
    if (!os.exists(path)) {
      println(s"Error: Input path does not exist: $inputPath")
      return
    }
    if (path.ext != "chester") {
      println(s"Error: Input path must be a .chester file: $inputPath")
      return
    }

    // Create a new SemanticDBGenerator instance
    val generator = new SemanticDBGenerator()

    // Process the input path
    generator.processPath(path)

    // Save the SemanticDB file
    val outputPath = path / os.up / (path.baseName + ".semanticdb")
    generator.saveSemanticDB(path.toString, outputPath.toString)

    println(s"SemanticDB generated at: $outputPath")
  }
}
