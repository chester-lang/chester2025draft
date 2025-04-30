package chester.tyck

import chester.reader.{*, given}
import munit.FunSuite
import chester.i18n.*
import chester.readerv2.ChesterReaderV2

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class FilesTyckFailsTest extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/tyck-fails")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(t"$baseName.expected")
      val expectedExists = Files.exists(expectedFile)

      ChesterReaderV2
        .parseTopLevel(FilePath(inputFile.toString))
        .fold(
          error => fail(t"Failed to parse file: $inputFile, error: $error"),
          parsedBlock =>
            Tycker.check(parsedBlock) match {
              case TyckResult.Success(_, _, _) =>
                fail(t"Expected file to fail type checking: $inputFile")
              case TyckResult.Failure(errors, _, _, _) =>
                val actual = errors.map(_.toString).mkString("\n")

                if (false) {
                  if (!expectedExists) {
                    Files.write(expectedFile, actual.getBytes)
                    println(t"Created expected file: $expectedFile")
                  } else {
                    val expected =
                      Files.readString(expectedFile, StandardCharsets.UTF_8)
                    assertEquals(actual, expected)
                  }
                }

            }
        )
    }
  }
}
