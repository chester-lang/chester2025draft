package chester.cli

import munit.FunSuite

import java.io.File

def splitFileName(file: File): (String, String) = {
  val name = file.getName
  val dotIndex = name.lastIndexOf('.')
  if (dotIndex > 0) (name.substring(0, dotIndex), name.substring(dotIndex + 1))
  else (name, "")
}
class CompileTests extends FunSuite {
  val dir: File = new File("tests/compile")
  val files: Array[File] = dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".chester"))
  for (file <- files)
    test(s"Compile ${file.getName}") {
      val path = file.toPath
      val (name, ext) = splitFileName(file)
      assertEquals(ext, "chester", s"File $name should have .chester extension")
      val comparePath = path.resolveSibling(s"$name.ts")
      val tempPath = path.resolveSibling(s"$name.temp.ts")
      println(s"Compiling $path ...")
      Main.main(
        Array(
          "compile",
          "--target=typescript",
          s"--output=$tempPath",
          path.toString
        )
      )
      val comparePathExists = comparePath.toFile.exists()
      if (comparePathExists) {
        val compareContent = scala.io.Source.fromFile(comparePath.toFile).mkString
        val tempContent = scala.io.Source.fromFile(tempPath.toFile).mkString
        assertEquals(
          tempContent,
          compareContent,
          s"Compiled output does not match expected output for file $name"
        )
      } else {
        // If the compare file does not exist, we can create it
        println(s"Creating expected output file: $comparePath")
        tempPath.toFile.renameTo(comparePath.toFile)
      }
    }
}
