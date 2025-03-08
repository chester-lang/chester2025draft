package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class PatternMatchingTest extends FunSuite {
  test("match") {
    val input =
      """
        |  notification match {
        |    case Email(sender, title, _) => A;
        |    case SMS(number, message) => B;
        |    case VoiceRecording(name, link) => C;
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
                Identifier("A", None)
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

  // The V2 parser handles newlines differently from V1
  // Keep using parseAndCheck (V1 only) for now
  test("match2") {
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
    parseAndCheck(input, expected)
  }
}
