package chester.utils

import munit.FunSuite
import spire.math.UInt

class StringIndexSuite extends FunSuite {
  test("charIndexToUnicodeIndex for simple characters") {
    val sp = StringIndex("hello")
    assertEquals(sp.charIndexToUnicodeIndex(UInt(0)).toInt, 0)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(4)).toInt, 4)
  }
  test("charIndexToUnicodeIndex for characters with surrogate pairs") {
    val sp = StringIndex("a\uD834\uDD1Eb")
    assertEquals(sp.charIndexToUnicodeIndex(UInt(0)).toInt, 0)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(1)).toInt, 1)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(2)).toInt, 1)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(3)).toInt, 2)
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
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(UInt(0), UInt(4)))
  }

  test("charIndexToCharLineAndColumn for multi-line string") {
    val sp = StringIndex("hello\nworld")
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(5), LineAndColumn(UInt(0), UInt(5)))
    assertEquals(sp.charIndexToCharLineAndColumn(6), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(11), LineAndColumn(UInt(1), UInt(5)))
  }

  test("charIndexToUnicodeLineAndColumn for single line string") {
    val sp = StringIndex("hello")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(UInt(0), UInt(4)))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line string with surrogate pairs"
  ) {
    val sp = StringIndex("hello\n\uD834\uDD1Eworld")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(UInt(0), UInt(5)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(8), LineAndColumn(UInt(1), UInt(1)))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line string with mixed characters"
  ) {
    val sp = StringIndex("hello\n\uD834\uDD1Eworld\nJava\uD834\uDD1EScala")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(UInt(0), UInt(5)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(8), LineAndColumn(UInt(1), UInt(1)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(13), LineAndColumn(UInt(1), UInt(6)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(14), LineAndColumn(UInt(2), UInt(0)))
    assertEquals(
      sp.charIndexToUnicodeLineAndColumn(20),
      LineAndColumn(UInt(2), UInt(5))
    ) // Tests the position after the surrogate pair
  }
  test("charIndexToUnicodeIndex for Chinese characters") {
    val sp = StringIndex("你好世界") // "Hello world" in Chinese
    assertEquals(sp.charIndexToUnicodeIndex(UInt(0)).toInt, 0)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(1)).toInt, 1)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(2)).toInt, 2)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(3)).toInt, 3)
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
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(1), LineAndColumn(UInt(0), UInt(1)))
    assertEquals(sp.charIndexToCharLineAndColumn(2), LineAndColumn(UInt(0), UInt(2)))
    assertEquals(sp.charIndexToCharLineAndColumn(3), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(UInt(1), UInt(1)))
  }

  test("charIndexToUnicodeLineAndColumn for multi-line Chinese string") {
    val sp = StringIndex("你好\n世界")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(1), LineAndColumn(UInt(0), UInt(1)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(2), LineAndColumn(UInt(0), UInt(2)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(3), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(UInt(1), UInt(1)))
  }
  test("charIndexToUnicodeIndex for Chinese characters with surrogate pairs") {
    val sp =
      StringIndex("𠀋好𠀌") // Includes rare Chinese characters outside the BMP
    assertEquals(sp.charIndexToUnicodeIndex(UInt(0)).toInt, 0)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(1)).toInt, 0)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(2)).toInt, 1)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(3)).toInt, 2)
    assertEquals(sp.charIndexToUnicodeIndex(UInt(4)).toInt, 2)
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
    assertEquals(sp.charIndexToCharLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(1), LineAndColumn(UInt(0), UInt(1)))
    assertEquals(sp.charIndexToCharLineAndColumn(2), LineAndColumn(UInt(0), UInt(2)))
    assertEquals(sp.charIndexToCharLineAndColumn(3), LineAndColumn(UInt(0), UInt(3)))
    assertEquals(sp.charIndexToCharLineAndColumn(4), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToCharLineAndColumn(5), LineAndColumn(UInt(1), UInt(1)))
    assertEquals(sp.charIndexToCharLineAndColumn(6), LineAndColumn(UInt(1), UInt(2)))
  }

  test(
    "charIndexToUnicodeLineAndColumn for multi-line Chinese string with surrogate pairs"
  ) {
    val sp = StringIndex("𠀋好\n𠀌世")
    assertEquals(sp.charIndexToUnicodeLineAndColumn(0), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(1), LineAndColumn(UInt(0), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(2), LineAndColumn(UInt(0), UInt(1)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(3), LineAndColumn(UInt(0), UInt(2)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(4), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(5), LineAndColumn(UInt(1), UInt(0)))
    assertEquals(sp.charIndexToUnicodeLineAndColumn(6), LineAndColumn(UInt(1), UInt(1)))
  }

}
