package chester.reader

import chester.readerv2.ReaderV2.DEBUG
import chester.syntax.concrete.*
import munit.FunSuite

class HardDef extends FunSuite{
  test("parse block with multiple statements without newlines") {
    val input = " { data Dog extends AnimalType { wo: String; };}"
    val expected =
      Block(
        statements = Vector(
          OpSeq(
            seq = Vector(
              Identifier(
                name = "data",
                meta = None
              ),
              Identifier(
                name = "Dog",
                meta = None
              ),
              Identifier(
                name = "extends",
                meta = None
              ),
              Identifier(
                name = "AnimalType",
                meta = None
              ),
              Block(
                statements = Vector(
                  OpSeq(
                    seq = Vector(
                      Identifier(
                        name = "wo",
                        meta = None
                      ),
                      Identifier(
                        name = ":",
                        meta = None
                      ),
                      Identifier(
                        name = "String",
                        meta = None
                      )
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
        ),
        result = None,
        meta = None
      )
    DEBUG.withValue(true) {
      parseAndCheckV1(input, expected)
    }
  }

}
