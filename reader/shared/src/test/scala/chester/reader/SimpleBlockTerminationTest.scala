package chester.reader

import chester.syntax.concrete.*
import munit.FunSuite
import chester.readerv2.ReaderV2
import chester.reader.parseAndCheckBoth

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

    ReaderV2.DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }
  }
  test("block with newline termination complex") {

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

    ReaderV2.DEBUG.withValue(false) {
      parseAndCheckBoth(input, expected)
    }

  }
}
