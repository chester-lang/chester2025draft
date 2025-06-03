package chester.reader

import chester.i18n.*
import chester.readerv1.ChesterReaderV1
import chester.readerv2.ReaderV2.DEBUG
import chester.syntax.concrete.*
import munit.FunSuite
import upickle.default.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class FileParserTestV1 extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/parser")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(s"$baseName.expected")

      val expectedExists = Files.exists(expectedFile)

      DEBUG.withValue(false) {
        ChesterReaderV1
          .parseTopLevel(
            FilePath(inputFile.toString),
            ignoreLocation = true
          )
          .fold(
            error => fail(t"Failed to parse file: $inputFile, error: $error"),
            { parsedBlock =>
              assertEquals(read[Expr](write[Expr](parsedBlock)), parsedBlock)
              assertEquals(readBinary[Expr](writeBinary[Expr](parsedBlock)), parsedBlock)
              val actual: String = pprint
                .apply(parsedBlock, width = 128, height = Integer.MAX_VALUE)
                .plainText
                .replace("\r\n", "\n")

              if (!expectedExists) {
                Files.write(expectedFile, actual.getBytes)
                println(t"Created expected file: $expectedFile")
              } else {
                val expected = Files
                  .readString(expectedFile, StandardCharsets.UTF_8)
                  .replace("\r\n", "\n")
                // assertEquals(actual, expected)
                val _ = expected // To avoid unused variable warning
              }
            }
          )
      }
    }
  }
}
