package chester.utils.cell

import chester.i18n.*
trait Cell[T] {
  def default: Option[T] = None

  /** stable means can only change once from None to a fixed Some value or always be a fixed value
   */
  def readStable: Option[T]

  def readUnstable: Option[T] = readStable

  def hasStableValue: Boolean = readStable.isDefined

  def noStableValue: Boolean = !hasStableValue

  def hasSomeValue: Boolean = readUnstable.isDefined

  def noAnyValue: Boolean = !hasSomeValue

  /** fill an unstable cell */
  def fill(newValue: T): Cell[T]
}

trait SeqCell[T] extends UnstableCell[Seq[T]] with NoFill[Seq[T]] {
  def add(newValue: T): SeqCell[T]
}

trait BaseMapCell[A, B] {
  def add(key: A, value: B): BaseMapCell[A, B]
}

trait UnstableCell[T] extends Cell[T] {
  override def readStable: Option[T] =
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )

  override def hasStableValue: Boolean =
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )

  override def noStableValue: Boolean =
    throw new UnsupportedOperationException(
      t"${getClass.getName} is not stable"
    )
}

trait NoFill[T] extends Cell[T] {
  override def fill(newValue: T): Cell[T] =
    throw new UnsupportedOperationException(
      t"${getClass.getName} cannot be filled"
    )
}

trait MapCell[A, B] extends UnstableCell[Map[A, B]] with BaseMapCell[A, B] with NoFill[Map[A, B]] {}

case class OnceCell[T](
                        value: Option[T] = None,
                        override val default: Option[T] = None
                      ) extends Cell[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): OnceCell[T] = {

    require(value.isEmpty)
    copy(value = Some(newValue))
  }
}

case class MutableCell[T](value: Option[T]) extends Cell[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): MutableCell[T] =
    copy(value = Some(newValue))
}

case class CollectionCell[T](value: Vector[T] = Vector.empty) extends SeqCell[T] {
  override def readUnstable: Option[Vector[T]] = Some(value)

  override def add(newValue: T): CollectionCell[T] =
    copy(value = value :+ newValue)
}

case class MappingCell[A, B](value: Map[A, B] = Map.empty[A, B]) extends MapCell[A, B] {
  override def readStable: Option[Map[A, B]] = Some(value)

  override def add(key: A, newValue: B): MappingCell[A, B] =
    copy(value = value + (key -> newValue))
}

case class LiteralCell[T](value: T) extends Cell[T] {
  override def readStable: Option[T] = Some(value)

  override def hasStableValue: Boolean = true

  override def fill(newValue: T): LiteralCell[T] =
    throw new UnsupportedOperationException("LiteralCell cannot be filled")
}

/** A cell that automatically provides a default value during zonking if no other propagator fills it. This is used to avoid "not covered by any
 * propagator" errors for cells that are allowed to have default values.
 *
 * @param defaultValue
 * The default value to use if no other propagator fills this cell
 * @param value
 * The current value (if any)
 */
case class DefaultValueCell[T](
                                defaultValue: T,
                                value: Option[T] = None
                              ) extends Cell[T] {
  override def readStable: Option[T] = value

  override val default: Option[T] = Some(defaultValue)

  override def fill(newValue: T): DefaultValueCell[T] =
    copy(value = Some(newValue))
}
