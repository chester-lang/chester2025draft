package chester.utils

import munit.FunSuite
import _root_.io.github.iltotore.iron._

class StringIndexSuite extends FunSuite {
  test("charIndexToUnicodeIndex for simple characters") {
    val sp = StringIndex("hello")
    assertEquals(sp.charIndexToUnicodeIndex(0), 0)
    assertEquals(sp.charIndexToUnicodeIndex(4), 4)
  }
  test("charIndexToUnicodeIndex for characters with surrogate pairs") {
    val sp = StringIndex("a\uD834\uDD1Eb")
    assertEquals(sp.charIndexToUnicodeIndex(0), 0)
    assertEquals(sp.charIndexToUnicodeIndex(1), 1)
    assertEquals(sp.charIndexToUnicodeIndex(2), 1)
    assertEquals(sp.charIndexToUnicodeIndex(3), 2)
  }

  test("unicodeIndexToCharIndex for simple characters") {
    val sp = StringIndex("hello")
    assertEquals(sp.unicodeIndexToCharIndex(0), 0)
    assertEquals(sp.unicodeIndexToCharIndex(4), 4)
  }

  test("unicodeIndexToCharIndex for characters with surrogate pairs") {
    val sp = StringIndex("a\uD834\uDD1Eb")
    assertEquals(sp.unicodeIndexToCharIndex(0), 0)
    assertEquals(sp.unicodeIndexToCharIndex(1), 1)
    assertEquals(sp.unicodeIndexToCharIndex(2), 3)
  }

  test("unicodeLength for simple characters") {
    val sp = StringIndex("hello")
    assertEquals(sp.unicodeLength, 5)
  }

  test("unicodeLength for characters with surrogate pairs") {
    val sp = StringIndex("a\uD834\uDD1Eb")
    assertEquals(sp.unicodeLength, 3)
  }

  test("charIndexToCharLineAndColumn for single line string") {
    val sp = StringIndex("hello")
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(0, 4))
  }

  test("charIndexToCharLineAndColumn for multi-line string") {
    val sp = StringIndex("hello\nworld")
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(5), LineAndColumn(0, 5))
    assertEquals(sp.charIndexToCharLineAndColumn(6), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(11), LineAndColumn(1, 5))
  }

  test("charIndexToUnicodeLineAndColumn for single line string") {
    val sp = StringIndex("hello")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(0, 4))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line string with surrogate pairs"
  ) {
    val sp = StringIndex("hello\n\uD834\uDD1Eworld")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(0, 5))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(8), LineAndColumn(1, 1))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line string with mixed characters"
  ) {
    val sp = StringIndex("hello\n\uD834\uDD1Eworld\nJava\uD834\uDD1EScala")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(0, 5))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(8), LineAndColumn(1, 1))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(13), LineAndColumn(1, 6))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(14), LineAndColumn(2, 0))
    assertEquals(
      sp.charIndexToUnicodeLineAndColumn(20),
      LineAndColumn(2, 5)
    ) // Tests the position after the surrogate pair
  }
  test("charIndexToUnicodeIndex for Chinese characters") {
    val sp = StringIndex("你好世界") // "Hello world" in Chinese
    assertEquals(sp.charIndexToUnicodeIndex(0), 0)
    assertEquals(sp.charIndexToUnicodeIndex(1), 1)
    assertEquals(sp.charIndexToUnicodeIndex(2), 2)
    assertEquals(sp.charIndexToUnicodeIndex(3), 3)
  }

  test("unicodeIndexToCharIndex for Chinese characters") {
    val sp = StringIndex("你好世界")
    assertEquals(sp.unicodeIndexToCharIndex(0), 0)
    assertEquals(sp.unicodeIndexToCharIndex(1), 1)
    assertEquals(sp.unicodeIndexToCharIndex(2), 2)
    assertEquals(sp.unicodeIndexToCharIndex(3), 3)
  }

  test("charIndexToCharLineAndColumn for multi-line Chinese string") {
    val sp = StringIndex("你好\n世界")
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(1), LineAndColumn(0, 1))
    assertEquals(sp.charIndexToCharLineAndColumn(2), LineAndColumn(0, 2))
    assertEquals(sp.charIndexToCharLineAndColumn(3), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(1, 1))
  }

  test("charIndexToUnicodeLineAndColumn for multi-line Chinese string") {
    val sp = StringIndex("你好\n世界")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(1), LineAndColumn(0, 1))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(2), LineAndColumn(0, 2))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(3), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(1, 1))
  }
  test("charIndexToUnicodeIndex for Chinese characters with surrogate pairs") {
    val sp =
      StringIndex("𠀋好𠀌") // Includes rare Chinese characters outside the BMP
    assertEquals(sp.charIndexToUnicodeIndex(0), 0)
    assertEquals(sp.charIndexToUnicodeIndex(1), 0)
    assertEquals(sp.charIndexToUnicodeIndex(2), 1)
    assertEquals(sp.charIndexToUnicodeIndex(3), 2)
    assertEquals(sp.charIndexToUnicodeIndex(4), 2)
  }

  test("unicodeIndexToCharIndex for Chinese characters with surrogate pairs") {
    val sp = StringIndex("𠀋好𠀌")
    assertEquals(sp.unicodeIndexToCharIndex(0), 0)
    assertEquals(sp.unicodeIndexToCharIndex(1), 2)
    assertEquals(sp.unicodeIndexToCharIndex(2), 3)
  }

  test(
    "charIndexToCharLineAndColumn for multi-line Chinese string with surrogate pairs"
  ) {
    val sp = StringIndex("𠀋好\n𠀌世")
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(1), LineAndColumn(0, 1))
    assertEquals(sp.charIndexToCharLineAndColumn(2), LineAndColumn(0, 2))
    assertEquals(sp.charIndexToCharLineAndColumn(3), LineAndColumn(0, 3))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToCharLineAndColumn(5), LineAndColumn(1, 1))
    assertEquals(sp.charIndexToCharLineAndColumn(6), LineAndColumn(1, 2))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line Chinese string with surrogate pairs"
  ) {
    val sp = StringIndex("𠀋好\n𠀌世")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(1), LineAndColumn(0, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(2), LineAndColumn(0, 1))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(3), LineAndColumn(0, 2))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(1, 0))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(1, 1))
  }

}
