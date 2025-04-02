package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.readerv2.LexerV2
import pprint.Tree

class SimpleBlockTerminationTest extends FunSuite {

  test("simple block with newline termination") {
    // Enable debug output
    val oldDebug = LexerV2.DEBUG
    //LexerV2.DEBUG = true

    // Custom pprinter with wider width and colors
    val myPPrinter = pprint.PPrinter.Color.copy(
      defaultWidth = 120,
      additionalHandlers = {
        case obj: AnyRef if obj.getClass.getName.startsWith("chester.syntax") =>
          Tree.Apply("Syntax", Iterator(Tree.Literal(obj.toString)))
      }
    )

    try {
      val input =
        """
          |test match {
          |  case A => {
          |    println("A");
          |  }
          |  case B => "B";
          |}
          |""".stripMargin

      val expected = OpSeq(
        Vector(
          Identifier("test", None),
          Identifier("match", None),
          Block(
            Vector(
              OpSeq(
                Vector(
                  Identifier("case", None),
                  Identifier("A", None),
                  Identifier("=>", None),
                  Block(
                    Vector(
                      FunctionCall(
                        Identifier("println", None),
                        Tuple(
                          Vector(
                            StringLiteral("A", None)
                          ),
                          None
                        ),
                        None
                      )
                    ),
                    None,
                    None
                  )
                ),
                None
              ),
              OpSeq(
                Vector(
                  Identifier("case", None),
                  Identifier("B", None),
                  Identifier("=>", None),
                  StringLiteral("B", None)
                ),
                None
              )
            ),
            None,
            None
          )
        ),
        None
      )

      // Parse with V1 parser
      val resultV1 = parseV1(input)
      println("\n===== V1 PARSER RESULT =====")
      myPPrinter.pprintln(resultV1)

      // Parse with V2 parser
      val resultV2 = parseV2(input)
      println("\n===== V2 PARSER RESULT =====")
      myPPrinter.pprintln(resultV2)

      // Deep AST inspection with pprint
      println("\n===== V1 AST STRUCTURE =====")
      pprint.log(resultV1, "V1 AST")

      println("\n===== V2 AST STRUCTURE =====")
      pprint.log(resultV2, "V2 AST")

      // Run the original check to see if it fails
      println("\n===== COMPARING RESULTS =====")
      parseAndCheckBoth(input, expected)

      // Simple equality check
      println("\n===== V1 VS V2 EQUALITY CHECK =====")
      println(s"V1 equals V2: ${resultV1 == resultV2}")
    } finally
      // Restore original debug setting
      LexerV2.DEBUG = oldDebug
  }
  test("block with newline termination complex") {
    // Enable debug output
    val oldDebug = LexerV2.DEBUG

    // Custom pprinter with wider width and colors
    val myPPrinter = pprint.PPrinter.Color.copy(
      defaultWidth = 120,
      additionalHandlers = {
        case obj: AnyRef if obj.getClass.getName.startsWith("chester.syntax") =>
          Tree.Apply("Syntax", Iterator(Tree.Literal(obj.toString)))
      }
    )

    try {
    val input =
      """
        |  notification match {
        |    case Email(sender, title, _) => {
        |      println(sender);
        |      println(title);
        |    }
        |    case SMS(number, message) => B;
        |    case VoiceRecording(name, link) => C;
        |    case _ => D;
        |  }
        |""".stripMargin
    val expected = OpSeq(
      Vector(
        Identifier("notification", None),
        Identifier("match", None),
        Block(
          Vector(
            OpSeq(
              Vector(
                Identifier("case", None),
                FunctionCall(
                  Identifier("Email", None),
                  Tuple(
                    Vector(
                      Identifier("sender", None),
                      Identifier("title", None),
                      Identifier("_", None)
                    ),
                    None
                  ),
                  None
                ),
                Identifier("=>", None),
                Block(
                  Vector(
                    FunctionCall(
                      Identifier("println", None),
                      Tuple(
                        Vector(
                          Identifier("sender", None)
                        ),
                        None
                      ),
                      None
                    ),
                    FunctionCall(
                      Identifier("println", None),
                      Tuple(
                        Vector(
                          Identifier("title", None)
                        ),
                        None
                      ),
                      None
                    )
                  ),
                  None,
                  None
                )
              ),
              None
            ),
            OpSeq(
              Vector(
                Identifier("case", None),
                FunctionCall(
                  Identifier("SMS", None),
                  Tuple(
                    Vector(
                      Identifier("number", None),
                      Identifier("message", None)
                    ),
                    None
                  ),
                  None
                ),
                Identifier("=>", None),
                Identifier("B", None)
              ),
              None
            ),
            OpSeq(
              Vector(
                Identifier("case", None),
                FunctionCall(
                  Identifier("VoiceRecording", None),
                  Tuple(
                    Vector(
                      Identifier("name", None),
                      Identifier("link", None)
                    ),
                    None
                  ),
                  None
                ),
                Identifier("=>", None),
                Identifier("C", None)
              ),
              None
            ),
            OpSeq(
              Vector(
                Identifier("case", None),
                Identifier("_", None),
                Identifier("=>", None),
                Identifier("D", None)
              ),
              None
            )
          ),
          None,
          None
        )
      ),
      None
    )

      // Parse with V1 parser
      val resultV1 = parseV1(input)
      println("\n===== V1 PARSER RESULT =====")
      myPPrinter.pprintln(resultV1)

      // Parse with V2 parser
      val resultV2 = parseV2(input)
      println("\n===== V2 PARSER RESULT =====")
      myPPrinter.pprintln(resultV2)

      // Deep AST inspection with pprint
      println("\n===== V1 AST STRUCTURE =====")
      pprint.log(resultV1, "V1 AST")

      println("\n===== V2 AST STRUCTURE =====")
      pprint.log(resultV2, "V2 AST")

      // Run the original check to see if it fails
      println("\n===== COMPARING RESULTS =====")

      LexerV2.DEBUG = true
      parseAndCheckBoth(input, expected)

      // Simple equality check
      println("\n===== V1 VS V2 EQUALITY CHECK =====")
      println(s"V1 equals V2: ${resultV1 == resultV2}")
    } finally
      // Restore original debug setting
      LexerV2.DEBUG = oldDebug
  }
}
