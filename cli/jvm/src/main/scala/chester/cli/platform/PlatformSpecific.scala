package chester.cli.platform

import chester.cli.Main.CliConfig
import chester.tyck.SemanticDBGenerator
import chester.utils.*
import chester.i18n.*

import scala.util.boundary
import scala.util.boundary.break

def genSemanticDB(config: CliConfig): Unit = boundary {
  val inputPath = config.input.getOrElse {
    println(t"Error: Input path is required.")
    break()
  }

  val path = os2.path(inputPath)
  if (!os.exists(path)) {
    println(t"Error: Input path does not exist: $inputPath")
    break()
  }
  if (path.ext != "chester") {
    println(t"Error: Input path must be a .chester file: $inputPath")
    break()
  }

  // Create a new SemanticDBGenerator instance
  val generator = new SemanticDBGenerator()

  // Process the input path
  generator.processPath(path)

  // Save the SemanticDB file
  val outputPath = path / os.up / (path.baseName + ".semanticdb")
  generator.saveSemanticDB(path.toString, outputPath.toString)

  println(t"SemanticDB generated at: $outputPath")
}

def testFunctionalities(): Unit = {
  println("functionalities test start")
  chester.scala.Test.callit()
  println("functionalities test end")
}
