package chester.uniqid

import upickle.default.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

private val uniqIdCounter = AtomicInteger(0)

type Uniqid = UniqidOf[Any]

private val rwUniqID: ReadWriter[UniqidOf[Any]] =
  readwriter[java.lang.Integer].bimap(_.id, x=>UniqidOf(x.toInt.refineUnsafe))

implicit inline def rwUniqIDOf[T]: ReadWriter[UniqidOf[T]] = rwUniqID.asInstanceOf[ReadWriter[UniqidOf[T]]]

case class UniqidOf[+A] private[uniqid] (id: Int :| Positive0) extends AnyVal {

}
type UniqidOffset = Int :| Positive0

private implicit val UniqIdOffsetRW: ReadWriter[UniqidOffset] =
  readwriter[java.lang.Integer].bimap(_.toInt, _.toInt.refineUnsafe)

extension (id: UniqidOffset) {
  def <=(that: UniqidOffset): Boolean = id <= that
  private[uniqid] def +(offset: Int): UniqidOffset = (id + offset).refineUnsafe
  private[uniqid] def -(offset: UniqidOffset): Int = id - offset
}

/** start <= x < end */
case class UniqIdRange(start: UniqidOffset, end: UniqidOffset) derives ReadWriter {
  require(start <= end, s"Invalid range: $start > $end")

  def size: Int :| Positive0 = (end - start).refineUnsafe
}

extension (x: Uniqid) {
  def asof[T]: UniqidOf[T] = x.asInstanceOf[UniqidOf[T]]
}

extension [T](x: UniqidOf[T]) {
  def asid: Uniqid = x
  def rerange(current: UniqIdRange, target: UniqIdRange): UniqidOf[T] = {
    require(
      current.start <= x.id && x.id < current.end,
      s"Invalid range: $current, $x"
    )
    val offset = x.id - current.start
    UniqidOf((target.start + offset).refineUnsafe)
  }
}

trait UCollector {
  def apply[T](x: UniqidOf[T]): Unit = ()
}

trait CollectUniqId extends Any {
  def collectU(collector: UCollector): Unit
}

trait UReplacer {
  def apply[T](x: UniqidOf[T]): UniqidOf[T] = x
}

trait RerangeUniqid extends Any {
  def replaceU(reranger: UReplacer): Any
}

trait ContainsUniqid extends Any with CollectUniqId with RerangeUniqid {
  lazy val uniqIdRange: UniqIdRange = Uniqid.calculateRange(this)

}

trait OnlyHasUniqid extends Any {
  def uniqId: Uniqid
}

trait HasUniqid extends Any with ContainsUniqid with OnlyHasUniqid {}

object Uniqid {
  def generate[T]: UniqidOf[T] = UniqidOf(uniqIdCounter.getAndIncrement().refineUnsafe)

  def requireRange(size: Int :| Positive0): UniqIdRange = {
    val start = uniqIdCounter.getAndAdd(size)
    UniqIdRange(start.refineUnsafe, (start + size).refineUnsafe)
  }

  def currentOffset(): UniqidOffset = uniqIdCounter.get().refineUnsafe

  def captureRange[T](f: => T): (UniqIdRange, T) = {
    val start = currentOffset()
    val result = f
    val end = currentOffset()
    (UniqIdRange(start, end), result)
  }

  def is(x: Any): Boolean = x.isInstanceOf[Int] || x.isInstanceOf[Integer]

  def calculateRange[T <: ContainsUniqid](x: T): UniqIdRange = {
    val currentRangeCollect = new mutable.ArrayDeque[Uniqid]()
    val collecter: UCollector = new UCollector {
      override def apply[T](id: UniqidOf[T]): Unit = {
        currentRangeCollect.append(id)
      }
    }
    x.collectU(collecter)
    UniqIdRange(currentRangeCollect.map(_.id).min, (currentRangeCollect.map(_.id).max + 1).refineUnsafe)
  }

  case class GiveNewRangeResult[T <: ContainsUniqid](
      oldRange: UniqIdRange,
      newRange: UniqIdRange,
      result: T
  )

  def giveNewRange[T <: ContainsUniqid](
      x: T
  ): GiveNewRangeResult[T] = {
    val currentRange = x.uniqIdRange
    val newRange = requireRange(currentRange.size)
    val reranger: UReplacer = new UReplacer {
      override def apply[U](id: UniqidOf[U]): UniqidOf[U] =
        id.rerange(currentRange, newRange)
    }
    GiveNewRangeResult(currentRange, newRange, x.replaceU(reranger).asInstanceOf[T])
  }
}
