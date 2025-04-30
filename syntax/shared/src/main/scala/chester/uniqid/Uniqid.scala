package chester.uniqid

import upickle.default.*
import chester.i18n.*
import spire.math.Natural
import chester.utils.impls.naturalRW
import chester.utils.{Nat, asInt}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

private val uniqIdCounter = new AtomicInteger(0)

type Uniqid = UniqidOf[Any]

private val rwUniqID: ReadWriter[UniqidOf[Any]] = readwriter[Int].bimap(
  _.id.asInt, // Convert Natural ID to Int for serialization
  x => UniqidOf(Nat(x)) // Convert Int back to Natural and wrap in UniqidOf
)

implicit inline def rwUniqIDOf[T]: ReadWriter[UniqidOf[T]] = rwUniqID.asInstanceOf[ReadWriter[UniqidOf[T]]]

case class UniqidOf[+A] private[uniqid] (id: spire.math.Natural) {}
type UniqidOffset = spire.math.Natural

extension (id: UniqidOffset) {
  def <=(that: UniqidOffset): Boolean = id <= that
  private[uniqid] def +(offset: Natural): UniqidOffset = id + offset // No refineUnsafe needed
  private[uniqid] def -(offset: UniqidOffset): Natural = id - offset // Return Natural
}

/** start <= x < end */
case class UniqIdRange(start: UniqidOffset, end: UniqidOffset) derives ReadWriter {
  require(start <= end, t"Invalid range: $start > $end")

  def size: spire.math.Natural = end - start // No refineUnsafe needed
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
    UniqidOf(target.start + offset) // Add Natural directly, offset is already Natural
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
  def generate[T]: UniqidOf[T] = UniqidOf(Nat(uniqIdCounter.getAndIncrement()))

  def requireRange(size: spire.math.Natural): UniqIdRange = { // size is Natural
    val sizeInt = size.asInt // Convert to Int for AtomicInteger
    val start = uniqIdCounter.getAndAdd(sizeInt) // Returns Int
    UniqIdRange(Nat(start), Nat(start + sizeInt)) // Convert results back to Natural
  }

  def currentOffset(): UniqidOffset = Nat(uniqIdCounter.get()) // Convert Int to Natural

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
    import spire.compat.ordering // Import ordering for Natural
    if (currentRangeCollect.isEmpty) UniqIdRange(currentOffset(), currentOffset())
    else UniqIdRange(currentRangeCollect.map(_.id).min, currentRangeCollect.map(_.id).max + Nat(1))
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
