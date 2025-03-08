package chester.reader

import chester.reader.*
import chester.syntax.concrete.*
import munit.FunSuite

class SimplePatternMatchingTest extends FunSuite {
  test("simple match") {
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
        Identifier("notification", meta = None),
        Identifier("match", meta = None),
        Block(
          statements = Vector(
            OpSeq(
              Vector(
                Identifier("case", meta = None),
                FunctionCall(
                  Identifier("Email", meta = None),
                  Tuple(
                    Vector(
                      Identifier("sender", meta = None),
                      Identifier("title", meta = None),
                      Identifier("_", meta = None)
                    ),
                    meta = None
                  ),
                  meta = None
                ),
                Identifier("=>", meta = None),
                Identifier("A", meta = None)
              ),
              meta = None
            ),
            OpSeq(
              Vector(
                Identifier("case", meta = None),
                FunctionCall(
                  Identifier("SMS", meta = None),
                  Tuple(
                    Vector(
                      Identifier("number", meta = None),
                      Identifier("message", meta = None)
                    ),
                    meta = None
                  ),
                  meta = None
                ),
                Identifier("=>", meta = None),
                Identifier("B", meta = None)
              ),
              meta = None
            ),
            OpSeq(
              Vector(
                Identifier("case", meta = None),
                FunctionCall(
                  Identifier("VoiceRecording", meta = None),
                  Tuple(
                    Vector(
                      Identifier("name", meta = None),
                      Identifier("link", meta = None)
                    ),
                    meta = None
                  ),
                  meta = None
                ),
                Identifier("=>", meta = None),
                Identifier("C", meta = None)
              ),
              meta = None
            )
          ),
          result = None,
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheckBoth(input, expected)
  }
}
