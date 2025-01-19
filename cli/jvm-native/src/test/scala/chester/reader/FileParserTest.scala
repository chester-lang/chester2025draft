package chester.reader

import chester.reader._
import chester.syntax.concrete._
import munit.Assertions.assertEquals
import munit.FunSuite
import upickle.default._

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class FileParserTest extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/parser")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(s"$baseName.expected")

      val expectedExists = Files.exists(expectedFile)

      ChesterReader.parseTopLevel(
        FilePath(inputFile.toString),
        ignoreLocation = true
      ).fold(error => fail(s"Failed to parse file: $inputFile, error: $error"), { parsedBlock => assertEquals(read[Expr](write[Expr](parsedBlock)), parsedBlock)
          assertEquals(readBinary[Expr](writeBinary[Expr](parsedBlock)), parsedBlock)
          val actual: String = pprint
            .apply(parsedBlock, width = 128, height = Integer.MAX_VALUE)
            .plainText
            .replace("\r\n", "\n")

          if (!expectedExists) {
            Files.write(expectedFile, actual.getBytes)
            println(s"Created expected file: $expectedFile")
          } else {
            val expected = Files
              .readString(expectedFile, StandardCharsets.UTF_8)
              .replace("\r\n", "\n")
            assertEquals(actual, expected)
          } })
    }
  }
}
