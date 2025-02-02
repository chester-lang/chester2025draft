package chester.reader

import chester.error.Reporter
import chester.readerv2.LexerV2
import chester.syntax.concrete.*
import munit.Assertions.{assertEquals, fail}
import upickle.default.*

def parseAndCheckV0(input: String, expected: Expr): Unit = {
  val resultignored = ChesterReader.parseExpr(
    FileNameAndContent("testFile", input)
  ) // it must parse with location
  ChesterReader
    .parseExpr(
      FileNameAndContent("testFile", input),
      ignoreLocation = true
    )
    .fold(
      error =>
        fail(
          s"Parsing failed for input: $input ${error.message} at index ${error.pos}"
        ),
      { value =>
        assertEquals(read[Expr](write[Expr](value)), value)
        assertEquals(
          read[Expr](write[Expr](resultignored.right.get)),
          resultignored.right.get
        )
        assertEquals(readBinary[Expr](writeBinary[Expr](value)), value)
        assertEquals(
          readBinary[Expr](writeBinary[Expr](resultignored.right.get)),
          resultignored.right.get
        )
        assertEquals(value, expected, s"Failed for input: $input")
      }
    )
}
@deprecated("TODO: remove this")
def getParsed(input: String): Expr = {
  ChesterReader.parseExpr(
    FileNameAndContent("testFile", input)
  ) // it must parse with location
  ChesterReader
    .parseExpr(
      FileNameAndContent("testFile", input),
      ignoreLocation = true
    )
    .fold(
      error =>
        fail(
          s"Parsing failed for input: $input ${error.message} at index ${error.pos}"
        ),
      value => value
    )
}

def parseAndCheck(input: String, expected: Expr): Unit = {
  // Print memory usage at start
  val runtime = Runtime.getRuntime
  def printMemoryUsage(label: String): Unit = {
    val mb = 1024 * 1024
    println(s"\n=== Memory Usage ($label) ===")
    println(s"Used Memory: ${(runtime.totalMemory - runtime.freeMemory) / mb}MB")
    println(s"Free Memory: ${runtime.freeMemory / mb}MB")
    println(s"Total Memory: ${runtime.totalMemory / mb}MB")
    println(s"Max Memory: ${runtime.maxMemory / mb}MB")
    System.out.flush()
  }

  println(s"\n========== Starting test for input (length=${input.length}): $input ==========")
  printMemoryUsage("Start")

  // Check old implementation first
  println("\n=== Testing old implementation ===")
  System.out.flush()
  try {
    parseAndCheckV0(input, expected)
    println("Old implementation passed")
  } catch {
    case e: Throwable =>
      println(s"Old implementation failed: ${e.getMessage}")
      e.printStackTrace()
  }
  printMemoryUsage("After old implementation")

  // Check new implementation
  println("\n=== Testing new implementation ===")
  System.out.flush()
  val source = FileNameAndContent("testFile", input)
  given reporter: Reporter[ParseError] = new Reporter[ParseError] {
    def apply(error: ParseError): Unit = {
      val errorIndex = error.pos.index.utf16
      val lineStart = input.lastIndexOf('\n', errorIndex) + 1
      val lineEnd = input.indexOf('\n', errorIndex) match {
        case -1 => input.length
        case n  => n
      }
      val line = input.substring(lineStart, lineEnd)
      val pointer = " " * (errorIndex - lineStart) + "^"
      
      fail(
        s"""Tokenizer error: ${error.message}
           |At position ${error.pos}:
           |$line
           |$pointer""".stripMargin)
    }
  }

  println("\n=== Creating source offset ===")
  System.out.flush()
  val sourceOffset = SourceOffset(source)
  printMemoryUsage("After source offset")

  println("\n=== Creating tokenizer ===")
  System.out.flush()
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  printMemoryUsage("After tokenizer creation")

  println("\n=== Tokenizing input ===")
  System.out.flush()
  val tokens = tokenizer.tokenize()
  println(s"Generated ${tokens.length} tokens")
  println("First 20 tokens:")
  tokens.take(20).foreach(token => println(s"  $token"))
  printMemoryUsage("After tokenization")
  
  // Store old debug flag value
  val oldDebug = chester.readerv2.LexerV2.DEBUG
  try {
    // Enable debug logging and flush output
    chester.readerv2.LexerV2.DEBUG = true
    
    println("\n=== Creating lexer ===")
    System.out.flush()
    val lexer = LexerV2(tokens, sourceOffset, ignoreLocation = true)
    printMemoryUsage("After lexer creation")

    println("\n=== Parsing expression ===")
    System.out.flush()
    val result = lexer
      .parseExpr()
      .fold(
        error => {
          println("\n=== Parsing failed ===")
          val errorIndex = error.pos.index.utf16
          val lineStart = input.lastIndexOf('\n', errorIndex) + 1
          val lineEnd = input.indexOf('\n', errorIndex) match {
            case -1 => input.length
            case n  => n
          }
          val line = input.substring(lineStart, lineEnd)
          val pointer = " " * (errorIndex - lineStart) + "^"
          
          fail(
            s"""V2 Parsing failed for input: $input
               |Error: ${error.message}
               |At position ${error.pos}:
               |$line
               |$pointer""".stripMargin)
        },
        { case (expr, state) => 
          println("\n=== Parsing succeeded ===")
          println(s"Successfully parsed expression: $expr")
          println(s"Expression type: ${expr.getClass.getName}")
          println(s"Remaining tokens: ${state.tokens.drop(state.index).take(5).mkString(", ")}")
          System.out.flush()
          expr 
        }
      )
    printMemoryUsage("After parsing")
    
    println("\n=== Comparing with expected result ===")
    println(s"Result type: ${result.getClass.getName}")
    println(s"Expected type: ${expected.getClass.getName}")
    println(s"Result: $result")
    println(s"Expected: $expected")
    System.out.flush()
    assertEquals(result, expected, s"Failed for input: $input")
    
    println("=== Test passed ===")
    printMemoryUsage("End")
    println("==========\n")
    System.out.flush()
  } catch {
    case e: Throwable =>
      println(s"\n=== Test failed with exception: ${e.getMessage} ===")
      e.printStackTrace()
      printMemoryUsage("After error")
      throw e
  } finally {
    // Restore original debug flag value
    chester.readerv2.LexerV2.DEBUG = oldDebug
  }
}

@deprecated("TODO: remove this")
def getParsedV1(input: String): Expr = {
  // Parse with location first to ensure it works
  ChesterReader.parseExpr(FileNameAndContent("testFile", input)).fold(
    error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
    _ => ()
  )
  
  // Then parse without location for the actual result
  ChesterReader.parseExpr(FileNameAndContent("testFile", input), ignoreLocation = true).fold(
    error => fail(s"Parsing failed for input: $input ${error.message} at index ${error.pos}"),
    value => value
  )
}
