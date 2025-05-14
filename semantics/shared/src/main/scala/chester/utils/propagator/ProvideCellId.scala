package chester.utils.propagator
import chester.utils.cell.*

// TODO: maybe distinguish between read and fill to have more sound Scala types and functions. One is +T and one is -T
trait ProvideCellId {
  type CIdOf[+T <: CellContent[?, ?]]
  type PIdOf[+T <: Propagator[?]]
  type CellId[T] = CIdOf[CellContentRW[T]]
  type CellIdAny = CIdOf[CellContent[?, ?]]
  type SeqId[T] = CIdOf[SeqCellContent[T, T]]
  type CellIdOr[T] = CellId[T] | T

  def isCId(x: Any): Boolean

  def assumeCId(x: Any): CellIdAny = x.asInstanceOf[CIdOf[CellContent[?, ?]]]

  def literal[T](t: T)(using state: StateOps[?]): CellId[T] = {
    val cell = state.addCell(LiteralCellContent[T](t))
    cell
  }

  /** Create a cell with a default value that will be used if no other propagator fills it. This is particularly useful for type variables that might
    * not be otherwise constrained.
    */
  def withDefault[T](defaultValue: T)(using state: StateOps[?]): CellId[T] = {
    val cell = state.addCell(DefaultValueCellContent[T](defaultValue))
    cell
  }

  private val NormalScore: Int = 8
  val NoScore: Int = 0

  trait Propagator[Ops] {
    def score: Int = NormalScore

    def identify: Option[Any] = None

    def readingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    def writingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    def defaultingCells(using StateRead[Ops], Ops): Set[CellIdAny] = Set.empty

    /** @return
      *   true if the propagator finished its work
      */
    def run(using StateOps[Ops], Ops): Boolean

    /** make a best guess for defaultingCells */
    def defaulting(
        needed: Vector[CIdOf[CellContent[?, ?]]]
    )(using StateOps[Ops], Ops): DefaultingResult

    def naiveFallbackZonk(
        needed: Vector[CIdOf[CellContent[?, ?]]]
    )(using StateOps[Ops], Ops): DefaultingResult =
      defaulting(needed)
  }

  trait StateRead[Ops] {
    def readCell[T <: CellContent[?, ?]](id: CIdOf[T]): Option[T]

    def readStable[U](id: CellId[U]): Option[U] =
      readCell[CellContentRW[U]](id).get.readStable
    def readUnstable[U](id: CellId[U]): Option[U] =
      readCell[CellContentRW[U]](id).get.readUnstable

    def hasStableValue[T <: CellContent[?, ?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasStableValue)

    def noStableValue[T <: CellContent[?, ?]](id: CIdOf[T]): Boolean = !hasStableValue(id)

    private def hasSomeValue[T <: CellContent[?, ?]](id: CIdOf[T]): Boolean =
      readCell(id).exists((x: T) => x.hasSomeValue)

    def noAnyValue[T <: CellContent[?, ?]](id: CIdOf[T]): Boolean = !hasSomeValue(id)

    def stable: Boolean

  }

  trait StateOps[Session] extends StateRead[Session] {
    protected def update[T <: CellContent[?, ?]](id: CIdOf[T], f: T => T)(using
                                                                          Session
    ): Unit

    def fill[T <: CellContentRW[U], U](id: CIdOf[T], f: U)(using Session): Unit =
      update[T](id, _.fill(f).asInstanceOf[T])

    def add[T <: SeqCellContent[U, U], U](id: CIdOf[T], f: U)(using Session): Unit =
      update[T](id, _.add(f).asInstanceOf[T])

    def add[T <: MapCellContent[A, B], A, B](id: CIdOf[T], key: A, value: B)(using
                                                                             Session
    ): Unit =
      update[T](id, _.add(key, value).asInstanceOf[T])

    def addCell[T <: CellContent[?, ?]](cell: T): CIdOf[T]

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
    def defaulting(cells: Vector[CIdOf[CellContent[?, ?]]])(using more: Session): Unit

    def toId[T](x: CellIdOr[T]): CIdOf[CellContentRW[T]] = x match {
      case x if isCId(x) => x.asInstanceOf[CIdOf[CellContentRW[T]]]
      case x =>
        val t = x.asInstanceOf[T]
        val cell = addCell(LiteralCellContent[T](t))
        cell.asInstanceOf[CIdOf[CellContentRW[T]]]
    }
  }

  enum DefaultingResult {
    case Done extends DefaultingResult
    case Require(needed: Seq[CIdOf[CellContent[?, ?]]]) extends DefaultingResult
    case NotYet extends DefaultingResult
  }
}
