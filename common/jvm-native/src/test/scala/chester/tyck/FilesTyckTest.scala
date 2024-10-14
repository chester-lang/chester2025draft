package chester.tyck

import chester.parser.{*, given}
import chester.syntax.concrete.*
import chester.syntax.core.Judge
import chester.utils.doc.*
import chester.utils.ponyfill.Files
import munit.FunSuite
import upickle.default.*

import java.nio.charset.StandardCharsets

class FilesTyckTest extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/tyck")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(s"$baseName.expected")
      val expectedExists = Files.exists(expectedFile)

      Parser.parseTopLevel(FilePath(inputFile.toString)) match {
        case Right(parsedBlock) =>
          assertEquals(read[Expr](write[Expr](parsedBlock)), parsedBlock)
          assertEquals(readBinary[Expr](writeBinary[Expr](parsedBlock)), parsedBlock)
          Tycker.check(parsedBlock) match {
            case TyckResult.Success(result, _, _) =>
              if(result.collectMeta.isEmpty) {
                println(s"Testing read/write for $inputFile")
                assertEquals(read[Judge](write[Judge](result)), result)
                assertEquals(readBinary[Judge](writeBinary[Judge](result)), result)
              } else {
                println(s"Skipping read/write test for $inputFile")
              }
              val actual = StringPrinter.render(result.wellTyped)(using
                PrettierOptions.Default
              )

              if (false) {
                if (!expectedExists) {
                  Files.write(expectedFile, actual.getBytes)
                  println(s"Created expected file: $expectedFile")
                } else {
                  val expected =
                    Files.readString(expectedFile, StandardCharsets.UTF_8)
                  assertEquals(actual, expected)
                }
              }

            case TyckResult.Failure(errors, _, _, _) =>
              fail(s"Failed to type check file: $inputFile, errors: $errors")
          }
        case Left(error) =>
          fail(s"Failed to parse file: $inputFile, error: $error")
      }
    }
  }
}

class FilesTyckFailsTest extends FunSuite {
  val (testDir, inputFiles) = getInputFiles("tests/tyck-fails")

  inputFiles.foreach { inputFile =>
    val baseName = inputFile.getFileName.toString.stripSuffix(".chester")
    test(baseName) {
      val expectedFile = testDir.resolve(s"$baseName.expected")
      val expectedExists = Files.exists(expectedFile)

      Parser.parseTopLevel(FilePath(inputFile.toString)) match {
        case Right(parsedBlock) =>
          Tycker.check(parsedBlock) match {
            case TyckResult.Success(_, _, _) =>
              fail(s"Expected file to fail type checking: $inputFile")
            case TyckResult.Failure(errors, _, _, _) =>
              val actual = errors.map(_.toString).mkString("\n")

              if (false) {
                if (!expectedExists) {
                  Files.write(expectedFile, actual.getBytes)
                  println(s"Created expected file: $expectedFile")
                } else {
                  val expected =
                    Files.readString(expectedFile, StandardCharsets.UTF_8)
                  assertEquals(actual, expected)
                }
              }

          }
        case Left(error) =>
          fail(s"Failed to parse file: $inputFile, error: $error")
      }
    }
  }
}
