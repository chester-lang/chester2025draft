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
          read[Expr](write[Expr](resultignored.getOrElse(throw new Exception("Failed to parse")))),
          resultignored.getOrElse(throw new Exception("Failed to parse"))
        )
        assertEquals(readBinary[Expr](writeBinary[Expr](value)), value)
        assertEquals(
          readBinary[Expr](writeBinary[Expr](resultignored.getOrElse(throw new Exception("Failed to parse")))),
          resultignored.getOrElse(throw new Exception("Failed to parse"))
        )
        assertEquals(value, expected, s"Failed for input: $input")
      }
    )
}

def parseAndCheck(input: String, expected: Expr): Unit = {
  // Check old implementation first
  parseAndCheckV0(input, expected)

  // Check new implementation
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
  val sourceOffset = SourceOffset(source)
  val tokenizer = chester.readerv2.Tokenizer(sourceOffset)
  val tokens = tokenizer.tokenize()
  
  // Store old debug flag value
  val oldDebug = chester.readerv2.LexerV2.DEBUG
  try {
    // Enable debug logging
    chester.readerv2.LexerV2.DEBUG = true
    println(s"\n=== Starting test for input: $input ===")
    println("Tokens:")
    tokens.take(20).foreach(token => println(s"  $token"))
    
    val lexer = LexerV2(tokens, sourceOffset, ignoreLocation = true)

    println("\nParsing expression...")
    val result = lexer
      .parseExpr()
      .fold(
        error => {
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
          println(s"Successfully parsed expression: $expr")
          println(s"Remaining tokens: ${state.tokens.drop(state.index).take(5).mkString(", ")}")
          expr 
        }
      )
    
    println("\nComparing with expected result...")
    assertEquals(result, expected, s"Failed for input: $input")
    println("=== Test passed ===\n")
  } finally {
    // Restore original debug flag value
    chester.readerv2.LexerV2.DEBUG = oldDebug
  }
}

def getParsed(input: String): Expr = {
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
