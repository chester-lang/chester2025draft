package chester.tyck

import chester.reader.{*, given}
import chester.utils.Debug
import chester.utils.Debug.DebugCategory
import munit.FunSuite

import java.nio.file.{Files, Paths}

/**
 * Test class for debugging the OnceCell bug in type-level function applications.
 */
class DebugTyckTest extends FunSuite {

  test("dependent-id-type-debug") {
    // Enable debug flags for all categories
    Debug.enable(DebugCategory.Cell)
    Debug.enable(DebugCategory.Tyck)
    Debug.enable(DebugCategory.Reducer)
    
    println("=== Starting debug test for dependent type OnceCell bug ===")
    
    // Get the test file path
    val inputFile = Paths.get("tests", "tyck", "dependent-id-type-debug.chester")
    assert(Files.exists(inputFile), s"Test file not found: $inputFile")
    
    // Parse the file
    println("=== Parsing file ===")
    ChesterReader
      .parseTopLevel(FilePath(inputFile.toString))
      .fold(
        error => fail(s"Failed to parse file: $inputFile, error: $error"),
        parsedBlock => {
          println("=== File parsed successfully ===")
          println(s"Parsed block: $parsedBlock")
          
          // Attempt to type check - this should trigger the bug
          println("=== Type checking ===")
          try {
            val result = Tycker.check(parsedBlock)
            println(s"Type check result: $result")
          } catch {
            case e: Exception => 
              println("=== Exception during type checking ===")
              println(s"Exception: ${e.getMessage}")
              e.printStackTrace()
              
              // We expect an IllegalArgumentException due to the OnceCell bug
              assert(e.isInstanceOf[IllegalArgumentException], "Expected IllegalArgumentException")
              assert(e.getMessage.contains("requirement failed"), "Expected 'requirement failed' message")
          }
        }
      )
      
    println("=== Debug test completed ===")
    
    // Disable debug flags
    Debug.disable(DebugCategory.Cell)
    Debug.disable(DebugCategory.Tyck)
    Debug.disable(DebugCategory.Reducer)
  }
}