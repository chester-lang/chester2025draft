package chester.utils.propagator
import chester.utils.cell.*

// TODO: maybe distinguish between read and fill to have more sound Scala types and functions. One is +T and one is -T
trait ProvideCellId {
  type CIdOf[+T <: CellRW[?, ?]]
  type PIdOf[+T <: Propagator[?]]
  type CellId[T] = CIdOf[Cell[T]]
  type CellIdAny = CIdOf[CellRW[?, ?]]
  type SeqId[T] = CIdOf[SeqCell[T]]
  type CellIdOr[T] = CellId[T] | T

  def isCId(x: Any): Boolean

  def assumeCId(x: Any): CellIdAny = x.asInstanceOf[CIdOf[CellRW[?, ?]]]

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

  trait Propagator[Ops] {
    def score: Int = NormalScore

    def identify: Option[Any] = None

    def readingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    def writingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    def zonkingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    /** @return
      *   true if the propagator finished its work
      */
    def run(using StateOps[Ops], Ops): Boolean

    /** make a best guess for zonkingCells */
    def zonk(
        needed: Vector[CIdOf[CellRW[?, ?]]]
    )(using StateOps[Ops], Ops): ZonkResult

    def naiveFallbackZonk(
        needed: Vector[CIdOf[CellRW[?, ?]]]
    )(using StateOps[Ops], Ops): ZonkResult =
      zonk(needed)
  }

  trait StateRead[Ops] {
    def readCell[T <: CellRW[?, ?]](id: CIdOf[T]): Option[T]

    def readStable[U](id: CellId[U]): Option[U] =
      readCell[Cell[U]](id).get.readStable
    def readUnstable[U](id: CellId[U]): Option[U] =
      readCell[Cell[U]](id).get.readUnstable

    def hasStableValue[T <: CellRW[?, ?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasStableValue)

    def noStableValue[T <: CellRW[?, ?]](id: CIdOf[T]): Boolean = !hasStableValue(id)

    private def hasSomeValue[T <: CellRW[?, ?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasSomeValue)

    def noAnyValue[T <: CellRW[?, ?]](id: CIdOf[T]): Boolean = !hasSomeValue(id)

    def stable: Boolean

  }

  trait StateOps[Session] extends StateRead[Session] {
    protected def update[T <: CellRW[?, ?]](id: CIdOf[T], f: T => T)(using
        Session
    ): Unit

    def fill[T <: Cell[U], U](id: CIdOf[T], f: U)(using Session): Unit =
      update[T](id, _.fill(f).asInstanceOf[T])

    def add[T <: SeqCell[U], U](id: CIdOf[T], f: U)(using Session): Unit =
      update[T](id, _.add(f).asInstanceOf[T])

    def add[T <: MapCell[A, B], A, B](id: CIdOf[T], key: A, value: B)(using
        Session
    ): Unit =
      update[T](id, _.add(key, value).asInstanceOf[T])

    def addCell[T <: CellRW[?, ?]](cell: T): CIdOf[T]

    def addPropagatorGetPid[T <: Propagator[Session]](propagator: T)(using
        more: Session
    ): PIdOf[T]

    final def addPropagator[T <: Propagator[Session]](propagator: T)(using
        Session
    ): Unit = {
      val _ = addPropagatorGetPid(propagator)
    }

    def tick(using Session): Unit

    def tickAll(using more: Session): Unit =
      while (!stable)
        tick(using more)

    /** make a best guess for those cells */
    def zonk(cells: Vector[CIdOf[CellRW[?, ?]]])(using more: Session): Unit

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
    case Require(needed: Seq[CIdOf[CellRW[?, ?]]]) extends ZonkResult
    case NotYet extends ZonkResult
  }
}
