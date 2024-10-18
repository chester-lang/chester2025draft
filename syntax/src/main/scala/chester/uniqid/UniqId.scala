package chester.uniqid

import upickle.default.*

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

private val uniqIdCounter = AtomicInteger(0)

opaque type UniqIdOf[+A] = Int

opaque type UniqIdOffset = Int

private implicit val UniqIdOffsetRW: ReadWriter[UniqIdOffset] =
  readwriter[java.lang.Integer].bimap(_.toInt, _.toInt)

extension (id: UniqIdOffset) {
  def <=(that: UniqIdOffset): Boolean = id <= that
  private[uniqid] def +(offset: Int): UniqIdOffset = id + offset
  private[uniqid] def -(offset: UniqIdOffset): Int = id - offset
}

/** start <= x < end */
case class UniqIdRange(start: UniqIdOffset, end: UniqIdOffset) derives ReadWriter {
  require(start <= end, s"Invalid range: $start > $end")

  def size: Int = end - start
}

type UniqId = UniqIdOf[Any]

extension (x: UniqId) {
  def asof[T]: UniqIdOf[T] = x.asInstanceOf[UniqIdOf[T]]
}

extension [T](x: UniqIdOf[T]) {
  def asid: UniqId = x
  def rerange(current: UniqIdRange, target: UniqIdRange): UniqIdOf[T] = {
    require(
      current.start <= x && x < current.end,
      s"Invalid range: $current, $x"
    )
    val offset = x - current.start
    target.start + offset
  }
}

private val rwUniqID: ReadWriter[UniqIdOf[?]] =
  readwriter[java.lang.Integer].bimap(_.toInt, _.toInt)

implicit inline def rwUniqIDOf[T]: ReadWriter[UniqIdOf[T]] = rwUniqID

trait UCollector {
  def apply[T](x: UniqIdOf[T]): Unit = ()
}

trait CollectUniqId extends Any {
  def collectU(collector: UCollector): Unit
}

trait UReplacer {
  def apply[T](x: UniqIdOf[T]): UniqIdOf[T] = x
}

trait RerangeUniqId extends Any {
  def replaceU(reranger: UReplacer): Any
}

trait ContainsUniqId extends Any with CollectUniqId with RerangeUniqId {
  lazy val uniqIdRange: UniqIdRange = UniqId.calculateRange(this)

}

trait OnlyHasUniqId extends Any {
  def uniqId: UniqId
}

trait HasUniqId extends Any with ContainsUniqId with OnlyHasUniqId {}

object UniqId {
  def generate[T]: UniqIdOf[T] = uniqIdCounter.getAndIncrement()

  def requireRange(size: Int): UniqIdRange = {
    require(size > 0, s"Invalid size: $size")
    val start = uniqIdCounter.getAndAdd(size)
    UniqIdRange(start, start + size)
  }

  def currentOffset(): UniqIdOffset = uniqIdCounter.get()

  def captureRange[T](f: => T): (UniqIdRange, T) = {
    val start = currentOffset()
    val result = f
    val end = currentOffset()
    (UniqIdRange(start, end), result)
  }

  def is(x: Any): Boolean = x.isInstanceOf[Int] || x.isInstanceOf[Integer] || x
    .isInstanceOf[UniqIdOf[?]]

  def calculateRange[T <: ContainsUniqId](x: T): UniqIdRange = {
    val currentRangeCollect = new mutable.ArrayDeque[UniqId]()
    val collecter: UCollector = new UCollector {
      override def apply[T](id: UniqIdOf[T]): Unit = {
        currentRangeCollect.append(id)
      }
    }
    x.collectU(collecter)
    UniqIdRange(currentRangeCollect.min, currentRangeCollect.max + 1)
  }

  case class GiveNewRangeResult[T <: ContainsUniqId](
      oldRange: UniqIdRange,
      newRange: UniqIdRange,
      result: T
  )

  def giveNewRange[T <: ContainsUniqId](
      x: T
  ): GiveNewRangeResult[T] = {
    val currentRange = x.uniqIdRange
    val newRange = requireRange(currentRange.size)
    val reranger: UReplacer = new UReplacer {
      override def apply[T](id: UniqIdOf[T]): UniqIdOf[T] =
        id.rerange(currentRange, newRange)
    }
    GiveNewRangeResult(currentRange, newRange, x.replaceU(reranger).asInstanceOf[T])
  }
}
