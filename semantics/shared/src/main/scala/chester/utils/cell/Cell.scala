package chester.utils.cell

import chester.i18n.*
trait Cell[+A, -B] {
  def default: Option[A] = None

  /** stable means can only change once from None to a fixed Some value or always be a fixed value
    */
  def readStable: Option[A]

  def readUnstable: Option[A] = readStable

  def hasStableValue: Boolean = readStable.isDefined

  def noStableValue: Boolean = !hasStableValue

  def hasSomeValue: Boolean = readUnstable.isDefined

  def noAnyValue: Boolean = !hasSomeValue

  /** fill an unstable cell */
  def fill(newValue: B): Cell[A, B]
}
type CellRW[A] = Cell[A, A]
type CellR[+A] = Cell[A, Nothing]
type CellW[-A] = Cell[Any, A]

trait SeqCell[+A, -B] extends UnstableCell[Seq[A], Seq[B]] with NoFill[Seq[A], Seq[B]] {
  def add(newValue: B): SeqCell[A, B]
}

trait BaseMapCell[A, B] {
  def add(key: A, value: B): BaseMapCell[A, B]
}

trait UnstableCell[+A, -B] extends Cell[A, B] {
  override def readStable: Option[A] =
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

trait NoFill[+A, -B] extends Cell[A, B] {
  override def fill(newValue: B): NoFill[A, B] =
    throw new UnsupportedOperationException(
      t"${getClass.getName} cannot be filled"
    )
}

trait MapCell[A, B] extends UnstableCell[Map[A, B], Map[A, B]] with BaseMapCell[A, B] with NoFill[Map[A, B], Map[A, B]] {}

case class OnceCell[T](
    value: Option[T] = None,
    override val default: Option[T] = None
) extends CellRW[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): OnceCell[T] = {

    require(value.isEmpty)
    copy(value = Some(newValue))
  }
}

case class MutableCell[T](value: Option[T]) extends CellRW[T] {
  override def readStable: Option[T] = value

  override def fill(newValue: T): MutableCell[T] =
    copy(value = Some(newValue))
}

case class CollectionCell[+A, -B <: A](value: Vector[A] = Vector.empty) extends SeqCell[A, B] {
  override def readUnstable: Option[Vector[A]] = Some(value)

  override def add(newValue: B): CollectionCell[A, B] =
    copy(value = value :+ newValue)
}

case class MappingCell[A, B](value: Map[A, B] = Map.empty[A, B]) extends MapCell[A, B] {
  override def readStable: Option[Map[A, B]] = Some(value)

  override def add(key: A, newValue: B): MappingCell[A, B] =
    copy(value = value + (key -> newValue))
}

case class LiteralCell[T](value: T) extends CellRW[T] {
  override def readStable: Option[T] = Some(value)

  override def hasStableValue: Boolean = true

  override def fill(newValue: T): LiteralCell[T] =
    throw new UnsupportedOperationException("LiteralCell cannot be filled")
}

/** A cell that automatically provides a default value during zonking if no other propagator fills it. This is used to avoid "not covered by any
  * propagator" errors for cells that are allowed to have default values.
  *
  * @param defaultValue
  *   The default value to use if no other propagator fills this cell
  * @param value
  *   The current value (if any)
  */
case class DefaultValueCell[T](
    defaultValue: T,
    value: Option[T] = None
) extends CellRW[T] {
  override def readStable: Option[T] = value

  override val default: Option[T] = Some(defaultValue)

  override def fill(newValue: T): DefaultValueCell[T] =
    copy(value = Some(newValue))
}
