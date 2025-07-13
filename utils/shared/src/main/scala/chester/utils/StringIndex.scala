package chester.utils

import fastparse.ParserInput
import _root_.io.github.iltotore.iron.constraint.all.*
import chester.i18n.*
import spire.math.Natural
import scala.language.experimental.genericNumberLiterals

case class LineAndColumn(
    line: spire.math.Natural,
    column: spire.math.Natural
)
case class LineAndColumnWithUTF16(line: spire.math.Natural, column: WithUTF16)

// Seq[String] may be a LazyList[String]
case class StringIndex(stringList: Seq[String]) {

  import java.lang.Character.{isHighSurrogate, isLowSurrogate}

  private def stringIterator: Iterator[Char] = stringList.iterator.flatten

  lazy val unicodeLength: Int =
    stringIterator.foldLeft(0)((count, char) => count + (if (isHighSurrogate(char)) 0 else 1))

  lazy val charLength: Int = stringIterator.foldLeft(0)((count, _) => count + 1)

  def charIndexToWithUTF16(charIndex: spire.math.Natural): WithUTF16 =
    WithUTF16(charIndexToUnicodeIndex(charIndex), charIndex)

  /** 0 <= charIndex <= charLength */
  def charIndexToUnicodeIndex(charIndex: spire.math.Natural): spire.math.Natural = {
    var index: Natural = 0
    var unicodeIndex: Natural = 0
    val it = stringIterator
    while (index < charIndex)
      if (it.hasNext) {
        val char = it.next()
        if (index < charIndex && isHighSurrogate(char)) {
          val nextChar = if (it.hasNext) it.next() else '\u0000'
          if (isLowSurrogate(nextChar)) {
            if (index + 1 < charIndex) {
              unicodeIndex = unicodeIndex + (1 : Natural)
              index = index + Nat(2)
            } else {
              index = index + (1 : Natural)
            }
          } else {
            unicodeIndex = unicodeIndex + (1 : Natural)
            index = index + (1 : Natural)
          }
        } else {
          unicodeIndex = unicodeIndex + (1 : Natural)
          index = index + (1 : Natural)
        }
      } else {
        throw new IllegalArgumentException(
          t"Index out of bounds (exceeds string length) $charIndex"
        )
      }
    unicodeIndex
  }

  def unicodeIndexToCharIndex(unicodeIndex: Int): Int = {
    var index = 0
    var unicodeCount = 0
    val it = stringIterator
    while (it.hasNext && unicodeCount < unicodeIndex) {
      val char = it.next()
      if (isHighSurrogate(char)) {
        val nextChar = if (it.hasNext) it.next() else '\u0000'
        if (isLowSurrogate(nextChar)) {
          unicodeCount += 1
          index += 2
        } else {
          unicodeCount += 1
          index += 1
        }
      } else {
        unicodeCount += 1
        index += 1
      }
    }
    index
  }

  /** 0 <= charIndex <= charLength */
  def charIndexToCharLineAndColumn(charIndex: Int): LineAndColumn = {
    var line = 0
    var column = 0
    var index = 0
    val it = stringIterator

    while (index < charIndex)
      if (it.hasNext) {
        val char = it.next()
        if (char == '\n') {
          line += 1
          column = 0
        } else {
          column += 1
        }
        index += 1
      } else {
        throw new IllegalArgumentException(
          t"Index out of bounds (exceeds string length) $charIndex"
        )
      }

    LineAndColumn(Nat(line), Nat(column))
  }

  def charIndexToLineAndColumnWithUTF16(
      charIndex: Int
  ): LineAndColumnWithUTF16 = {
    val utf16 = charIndexToCharLineAndColumn(charIndex)
    val unicode = charIndexToUnicodeLineAndColumn(charIndex)
    assert(utf16.line == unicode.line)
    LineAndColumnWithUTF16(
      unicode.line,
      WithUTF16(unicode.column, utf16.column)
    )
  }

  /** 0 <= charIndex <= charLength */
  def charIndexToUnicodeLineAndColumn(charIndex: Int): LineAndColumn = {
    if (charIndex < 0)
      throw new IllegalArgumentException(
        t"Index out of bounds (negative) $charIndex"
      )

    var line = 0
    var column = 0
    var index = 0
    val it = stringIterator

    while (index < charIndex)
      if (it.hasNext) {
        val char = it.next()
        if (char == '\n') {
          line += 1
          column = 0 // Reset column to 0 for the start of a new line
        } else {
          if (isHighSurrogate(char)) {
            val nextChar = if (it.hasNext) it.next() else '\u0000'
            if (isLowSurrogate(nextChar)) {
              if (index + 1 < charIndex) {
                column += 1
                index += 1 // Skip the low surrogate
              }
            } else {
              column += 1
            }
          } else {
            column += 1
          }
        }
        index += 1
      } else {
        throw new IllegalArgumentException(
          t"Index out of bounds (exceeds string length) $charIndex"
        )
      }

    LineAndColumn(Nat(line), Nat(column))
  }

}

object StringIndex {
  def apply(s: String): StringIndex = StringIndex(LazyList(s))

  def apply(iterator: Iterator[String]): StringIndex = {
    val lazyList = LazyList.from(iterator)
    StringIndex(lazyList)
  }

  def apply(parserInput: ParserInput): StringIndex =
    StringIndex(parserInputToLazyList(parserInput))
}
