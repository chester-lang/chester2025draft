package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite

class SimpleBlockTerminationTest extends FunSuite {

  test("simple block with newline termination") {
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
    println(resultV1)
    
    // Parse with V2 parser
    val resultV2 = parseV2(input)
    println("\n===== V2 PARSER RESULT =====")
    println(resultV2)
    
    // Run the original check to see if it fails
    println("\n===== COMPARING RESULTS =====")
    parseAndCheck(input, expected)
    
    // Simple equality check
    println("\n===== V1 VS V2 EQUALITY CHECK =====")
    println(s"V1 equals V2: ${resultV1 == resultV2}")
  }
}
