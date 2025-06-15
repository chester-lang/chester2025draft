package chester.readerv3

import chester.error.{Span, SpanInFile}
import chester.reader.{Offset, ParseError, Source}
import chester.readerv2.StringChar
import chester.utils.getCodePoints

import scala.collection.mutable.ArrayBuffer

object CharReader {
  private def reading(strings: Seq[String], source: Source, offset: Offset): Seq[StringChar] = {
    if(strings.isEmpty) return Seq.empty
    val head = strings.head
    val tail = strings.tail
    val chars = ArrayBuffer.empty[StringChar]
    var currentOffset = offset
    for(codePoint <- getCodePoints(head)) {
      val begin = currentOffset
      currentOffset = begin.next(codePoint)
      chars += StringChar(codePoint, Span(source, SpanInFile(currentOffset.getPos, currentOffset.getPos)))
    }
    LazyList.from(chars) ++ reading(tail, source, currentOffset)
  }
  def read(source: Source): Either[ParseError, Seq[StringChar]] = source.readContent match {
    case Left(error) => Left(error)
    case Right(strings) => Right(reading(strings, source, source.offset))
  }
}
