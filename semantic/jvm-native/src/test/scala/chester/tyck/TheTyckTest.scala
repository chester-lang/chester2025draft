package chester.tyck

import chester.i18n.*
import chester.reader.{*, given}
import chester.syntax.concrete.*
import chester.syntax.core.Judge
import chester.utils.doc.*
import munit.FunSuite
import upickle.default.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chester.utils.Debug
import chester.utils.Debug.DebugCategory

class TheTyckTest extends FunSuite {
  val baseName = "tyck"
  val testDir = Paths.get("tests")
  val inputFile = Paths.get("tests/tyck.chester")
  val doTest = true
  test(baseName) {
    if(doTest){

      val expectedFile = testDir.resolve(t"$baseName.expected")
      val expectedExists = Files.exists(expectedFile)

      ChesterReader
        .parseTopLevel(FilePath(inputFile.toString))
        .fold(
          error => fail(t"Failed to parse file: $inputFile, error: $error"),
          { parsedBlock =>
            assertEquals(read[Expr](write[Expr](parsedBlock)), parsedBlock)
            assertEquals(readBinary[Expr](writeBinary[Expr](parsedBlock)), parsedBlock)
            Debug.withCategoriesEnabled(Set(DebugCategory.TraitMatching)) {
              Tycker.check(parsedBlock) match {
                case TyckResult.Success(result, _, _) =>
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

                case TyckResult.Failure(errors, _, _, _) =>
                  fail(t"Failed to type check file: $inputFile, errors: $errors")
              }
            }
          }
        )
    }
  }
}
