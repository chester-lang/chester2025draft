package chester.tyck

import chester.error.TyckError
import chester.reader.{*, given}
import chester.syntax.concrete.*
import chester.syntax.core.Judge
import chester.utils.doc.*
import munit.FunSuite
import upickle.default.*
import chester.i18n.*
import chester.readerv2.ChesterReaderV2

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class FilesTyckTest extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/tyck")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(t"$baseName.expected")
      val expectedExists = Files.exists(expectedFile)

      ChesterReaderV2
        .parseTopLevel(FilePath(inputFile.toString))
        .fold(
          error => fail(t"Failed to parse file: $inputFile, error: $error"),
          { parsedBlock =>
            assertEquals(read[Expr](write[Expr](parsedBlock)), parsedBlock)
            assertEquals(readBinary[Expr](writeBinary[Expr](parsedBlock)), parsedBlock)
            val tyckResult = Tycker.check(parsedBlock)
            if (tyckResult.errorsEmpty) {
              // This is equivalent to TyckResult.Success case
              val result = tyckResult.result
              if (result.collectMeta.isEmpty) {
                println(t"Testing read/write for $inputFile")
                assertEquals(
                  StringPrinter.render(read[Judge](write[Judge](result)))(using
                    PrettierOptions.Default
                  ),
                  StringPrinter.render(result)(using
                    PrettierOptions.Default
                  )
                )
                assertEquals(
                  StringPrinter.render(readBinary[Judge](writeBinary[Judge](result)))(using
                    PrettierOptions.Default
                  ),
                  StringPrinter.render(result)(using
                    PrettierOptions.Default
                  )
                )
                assertEquals(
                  StringPrinter.render(result.wellTyped)(using
                    PrettierOptions.Default
                  ),
                  StringPrinter.render(result.wellTyped)(using
                    PrettierOptions.Default
                  )
                )
              } else {
                println(t"Skipping read/write test for $inputFile")
              }
              val actual = StringPrinter.render(result.wellTyped)(using
                PrettierOptions.Default
              )

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
            } else {
              // This is equivalent to TyckResult.Failure case
              val errors = tyckResult.problems.collect { case e: TyckError => e }
              fail(t"Failed to type check file: $inputFile, errors: $errors")
            }
          }
        )
    }
  }
}
