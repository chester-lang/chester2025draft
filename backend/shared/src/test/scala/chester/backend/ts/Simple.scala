package chester.backend.ts

import chester.reader.FileNameAndContent
import chester.readerv1.ChesterReaderV1
import munit.FunSuite
import chester.error.reporterToEither

class Simple extends FunSuite {
  test("compile simple assignment") {
    val input = "let x = 42;"
    val parsed = reporterToEither(ChesterReaderV1.parseTopLevel(FileNameAndContent("test.chester", input)))
    assert(parsed.isRight, s"Parsing failed: ${parsed.left.getOrElse("Unknown error")}")
    val result = parsed.toOption.get
    // TODO
  }

}
