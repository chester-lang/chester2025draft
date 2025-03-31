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
    parseAndCheck(input, expected)
  }
}
