package chester.parser

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

def getInputFiles(testDir: String): (Path, Seq[Path]) = {
  val path = Paths.get(testDir)

  val inputFiles = Files
    .list(path)
    .iterator()
    .asScala
    .filter(_.toString.endsWith(".chester"))
    .toSeq

  (path, inputFiles)
}
