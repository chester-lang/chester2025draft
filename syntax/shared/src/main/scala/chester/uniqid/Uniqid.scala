package chester.uniqid

import upickle.default.*
import _root_.io.github.iltotore.iron.*
import _root_.io.github.iltotore.iron.constraint.all.*
import _root_.io.github.iltotore.iron.constraint.numeric.*
import chester.i18n.*
import spire.math.UInt
import chester.utils.impls.uintRW

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

private val uniqIdCounter = new AtomicInteger(0)

type Uniqid = UniqidOf[Any]

private val rwUniqID: ReadWriter[UniqidOf[Any]] = readwriter[Int].bimap(
  _.id.toInt, // Convert UInt ID to Int for serialization
  x => UniqidOf(UInt(x)) // Convert Int back to UInt and wrap in UniqidOf
)

implicit inline def rwUniqIDOf[T]: ReadWriter[UniqidOf[T]] = rwUniqID.asInstanceOf[ReadWriter[UniqidOf[T]]]

case class UniqidOf[+A] private[uniqid] (id: spire.math.UInt) {}
type UniqidOffset = spire.math.UInt

extension (id: UniqidOffset) {
  def <=(that: UniqidOffset): Boolean = id <= that
  private[uniqid] def +(offset: UInt): UniqidOffset = id + offset // No refineUnsafe needed
  private[uniqid] def -(offset: UniqidOffset): UInt = id - offset // Return UInt
}

/** start <= x < end */
case class UniqIdRange(start: UniqidOffset, end: UniqidOffset) derives ReadWriter {
  require(start <= end, t"Invalid range: $start > $end")

  def size: spire.math.UInt = end - start // No refineUnsafe needed
}

extension (x: Uniqid) {
  def asof[T]: UniqidOf[T] = x.asInstanceOf[UniqidOf[T]]
}

extension [T](x: UniqidOf[T]) {
  def asid: Uniqid = x
  def rerange(current: UniqIdRange, target: UniqIdRange): UniqidOf[T] = {
    require(
      current.start <= x.id && x.id < current.end,
      t"Invalid range: $current, $x"
    )
    val offset = x.id - current.start
    UniqidOf(target.start + offset) // Add UInt directly, offset is already UInt
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
  def generate[T]: UniqidOf[T] = UniqidOf(UInt(uniqIdCounter.getAndIncrement()))

  def requireRange(size: spire.math.UInt): UniqIdRange = { // size is UInt
    val sizeInt = size.toInt // Convert to Int for AtomicInteger
    val start = uniqIdCounter.getAndAdd(sizeInt) // Returns Int
    UniqIdRange(UInt(start), UInt(start + sizeInt)) // Convert results back to UInt
  }

  def currentOffset(): UniqidOffset = UInt(uniqIdCounter.get()) // Convert Int to UInt

  def captureRange[T](f: => T): (UniqIdRange, T) = {
    val start = currentOffset()
    val result = f
    val end = currentOffset()
    (UniqIdRange(start, end), result)
  }

  def is(x: Any): Boolean = x match {
    case UniqidOf(_) => true
    case _           => false
  }

  def calculateRange[T <: ContainsUniqid](x: T): UniqIdRange = {
    val currentRangeCollect = new mutable.ArrayDeque[Uniqid]()
    val collecter: UCollector = new UCollector {
      override def apply[T](id: UniqidOf[T]): Unit =
        currentRangeCollect.append(id)
    }
    x.collectU(collecter)
    import spire.compat.ordering // Import ordering for UInt
    if (currentRangeCollect.isEmpty) UniqIdRange(currentOffset(), currentOffset())
    else UniqIdRange(currentRangeCollect.map(_.id).min, currentRangeCollect.map(_.id).max + UInt(1))
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
