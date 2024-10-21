package chester.parser

import chester.parser.*
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
        Identifier("notification", meta = None),
        Identifier("match", meta = None),
        Block(
          heads = Vector(
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
          tail = None,
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }

  // TODO
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
        Identifier("notification", meta = None),
        Identifier("match", meta = None),
        Block(
          heads = Vector(
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
                Block(
                  heads = Vector(
                    FunctionCall(
                      Identifier("println", meta = None),
                      Tuple(
                        Vector(Identifier("sender", meta = None)),
                        meta = None
                      ),
                      meta = None
                    ),
                    FunctionCall(
                      Identifier("println", meta = None),
                      Tuple(
                        Vector(Identifier("title", meta = None)),
                        meta = None
                      ),
                      meta = None
                    )
                  ),
                  tail = None,
                  meta = None
                )
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
            ),
            OpSeq(
              Vector(
                Identifier("case", meta = None),
                Identifier("_", meta = None),
                Identifier("=>", meta = None),
                Identifier("D", meta = None)
              ),
              meta = None
            )
          ),
          tail = None,
          meta = None
        )
      ),
      meta = None
    )
    parseAndCheck(input, expected)
  }
}
