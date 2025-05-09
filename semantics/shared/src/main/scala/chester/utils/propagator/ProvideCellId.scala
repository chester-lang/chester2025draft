package chester.utils.propagator
import chester.i18n.*

// TODO: maybe distinguish between read and fill to have more sound Scala types and functions. One is +T and one is -T
trait ProvideCellId {
  type CIdOf[+T <: Cell[?]]
  type PIdOf[+T <: Propagator[?]]
  type CellId[T] = CIdOf[Cell[T]]
  type CellIdAny = CIdOf[Cell[?]]
  type SeqId[T] = CIdOf[SeqCell[T]]
  type CellIdOr[T] = CellId[T] | T

  def isCId(x: Any): Boolean

  def assumeCId(x: Any): CellIdAny = x.asInstanceOf[CIdOf[Cell[?]]]

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
    *   The default value to use if no other propagator fills this cell
    * @param value
    *   The current value (if any)
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

  def literal[T](t: T)(using state: StateOps[?]): CellId[T] = {
    val cell = state.addCell(LiteralCell[T](t))
    cell
  }

  /** Create a cell with a default value that will be used if no other propagator fills it. This is particularly useful for type variables that might
    * not be otherwise constrained.
    */
  def withDefault[T](defaultValue: T)(using state: StateOps[?]): CellId[T] = {
    val cell = state.addCell(DefaultValueCell[T](defaultValue))
    cell
  }

  private val NormalScore: Int = 8
  val NoScore: Int = 0

  trait Propagator[Ability] {
    def score: Int = NormalScore

    def identify: Option[Any] = None

    def readingCells: Set[CIdOf[Cell[?]]] = Set.empty

    def writingCells: Set[CIdOf[Cell[?]]] = Set.empty

    def zonkingCells: Set[CIdOf[Cell[?]]] = Set.empty

    /** @return
      *   true if the propagator finished its work
      */
    def run(using  StateOps[Ability], Ability): Boolean

    /** make a best guess for zonkingCells */
    def zonk(
        needed: Vector[CIdOf[Cell[?]]]
    )(using StateOps[Ability], Ability): ZonkResult

    def naiveFallbackZonk(
        needed: Vector[CIdOf[Cell[?]]]
    )(using StateOps[Ability], Ability): ZonkResult =
      zonk(needed)
  }

  trait StateOps[Ability] {
    def readCell[T <: Cell[?]](id: CIdOf[T]): Option[T]

    def readStable[U](id: CellId[U]): Option[U] =
      readCell[Cell[U]](id).get.readStable
    def readUnstable[U](id: CellId[U]): Option[U] =
      readCell[Cell[U]](id).get.readUnstable

    protected def update[T <: Cell[?]](id: CIdOf[T], f: T => T)(using
        Ability
    ): Unit

    def fill[T <: Cell[U], U](id: CIdOf[T], f: U)(using Ability): Unit =
      update[T](id, _.fill(f).asInstanceOf[T])

    def add[T <: SeqCell[U], U](id: CIdOf[T], f: U)(using Ability): Unit =
      update[T](id, _.add(f).asInstanceOf[T])

    def add[T <: MapCell[A, B], A, B](id: CIdOf[T], key: A, value: B)(using
        Ability
    ): Unit =
      update[T](id, _.add(key, value).asInstanceOf[T])

    def addCell[T <: Cell[?]](cell: T): CIdOf[T]

    def hasStableValue[T <: Cell[?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasStableValue)

    def noStableValue[T <: Cell[?]](id: CIdOf[T]): Boolean = !hasStableValue(id)

    private def hasSomeValue[T <: Cell[?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasSomeValue)

    def noAnyValue[T <: Cell[?]](id: CIdOf[T]): Boolean = !hasSomeValue(id)

    def addPropagatorGetPid[T <: Propagator[Ability]](propagator: T)(using
        more: Ability
    ): PIdOf[T]

    final def addPropagator[T <: Propagator[Ability]](propagator: T)(using
        Ability
    ): Unit = {
      val _ = addPropagatorGetPid(propagator)
    }

    def tick(using more: Ability): Unit

    def stable: Boolean

    def tickAll(using more: Ability): Unit =
      while (!stable)
        tick(using more)

    /** make a best guess for those cells */
    def zonk(cells: Vector[CIdOf[Cell[?]]])(using more: Ability): Unit

    def toId[T](x: CellIdOr[T]): CIdOf[Cell[T]] = x match {
      case x if isCId(x) => x.asInstanceOf[CIdOf[Cell[T]]]
      case x =>
        val t = x.asInstanceOf[T]
        val cell = addCell(LiteralCell[T](t))
        cell.asInstanceOf[CIdOf[Cell[T]]]
    }
  }

  enum ZonkResult {
    case Done extends ZonkResult
    case Require(needed: Seq[CIdOf[Cell[?]]]) extends ZonkResult
    case NotYet extends ZonkResult
  }
}
