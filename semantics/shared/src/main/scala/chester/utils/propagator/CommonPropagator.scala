package chester.utils.propagator
import cats.implicits.*

trait CommonPropagator[Ck] extends ProvideCellId {

  case class MergeSimple[T](a: CellId[T], b: CellId[T]) extends Propagator[Ck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = Set(a, b)
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(a, b)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(a, b)

    override def run(using state: StateWith[Ck], more: Ck): Boolean = {
      val aVal = state.readStable(a)
      val bVal = state.readStable(b)
      if (aVal.isDefined && bVal.isDefined) {
        if (aVal.get == bVal.get) return true
        throw new IllegalStateException(
          "Merge propagator should not be used if the values are different"
        )
      }
      if (aVal.isDefined) {
        state.fill(b, aVal.get)
        return true
      }
      if (bVal.isDefined) {
        state.fill(a, bVal.get)
        return true
      }
      false
    }

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateWith[Ck], more: Ck): ZonkResult = {
      val aVal = state.readStable(a)
      val bVal = state.readStable(b)
      if (aVal.isDefined && bVal.isDefined) {
        if (aVal.get == bVal.get) return ZonkResult.Done
        throw new IllegalStateException(
          "Merge propagator should not be used if the values are different"
        )
      }
      if (aVal.isDefined) {
        state.fill(b, aVal.get)
        return ZonkResult.Done
      }
      if (bVal.isDefined) {
        state.fill(a, bVal.get)
        return ZonkResult.Done
      }
      ZonkResult.NotYet
    }
  }

  private case class FlatMaping[T, U](
      xs: Seq[CellId[T]],
      f: Seq[T] => U,
      result: CellId[U]
  ) extends Propagator[Ck] {
    override val readingCells: Set[CIdOf[Cell[?]]] = xs.toSet
    override val writingCells: Set[CIdOf[Cell[?]]] = Set(result)
    override val zonkingCells: Set[CIdOf[Cell[?]]] = Set(result)

    override def run(using state: StateWith[Ck], more: Ck): Boolean =
      xs.traverse(state.readStable).map(f).exists { result =>
        state.fill(this.result, result)
        true
      }

    override def zonk(
        needed: Vector[CellIdAny]
    )(using state: StateWith[Ck], more: Ck): ZonkResult = {
      val needed = xs.filter(state.noStableValue(_))
      if (needed.nonEmpty) return ZonkResult.Require(needed)
      val done = run
      require(done)
      ZonkResult.Done
    }
  }

  private def FlatMap[T, U](
      xs: Seq[CellId[T]]
  )(f: Seq[T] => U)(using ck: Ck, state: StateWith[Ck]): CellId[U] = {
    val cell = state.addCell(OnceCell[U]())
    state.addPropagator(FlatMaping(xs, f, cell))
    cell
  }

  def Map1[T, U](
      x: CellId[T]
  )(f: T => U)(using ck: Ck, state: StateWith[Ck]): CellId[U] = {
    val cell = state.addCell(OnceCell[U]())
    state.addPropagator(FlatMaping(Vector(x), (xs: Seq[T]) => f(xs.head), cell))
    cell
  }

  def Map2[A, B, C](x: CellId[A], y: CellId[B])(
      f: (A, B) => C
  )(using ck: Ck, state: StateWith[Ck]): CellId[C] = {
    val cell = state.addCell(OnceCell[C]())
    state.addPropagator(
      FlatMaping(
        Vector[CellId[Any]](
          x.asInstanceOf[CellId[Any]],
          y.asInstanceOf[CellId[Any]]
        ),
        (xs: Seq[Any]) => f(xs.head.asInstanceOf[A], xs(1).asInstanceOf[B]),
        cell
      )
    )
    cell
  }

  def Map3[A, B, C, D](x: CellId[A], y: CellId[B], z: CellId[C])(
      f: (A, B, C) => D
  )(using ck: Ck, state: StateWith[Ck]): CellId[D] = {
    val cell = state.addCell(OnceCell[D]())
    state.addPropagator(
      FlatMaping(
        Vector[CellId[Any]](
          x.asInstanceOf[CellId[Any]],
          y.asInstanceOf[CellId[Any]],
          z.asInstanceOf[CellId[Any]]
        ),
        (xs: Seq[Any]) =>
          f(
            xs.head.asInstanceOf[A],
            xs(1).asInstanceOf[B],
            xs(2).asInstanceOf[C]
          ),
        cell
      )
    )
    cell
  }

  def Traverse[A](
      x: Seq[CellId[A]]
  )(using Ck, StateWith[Ck]): CellId[Seq[A]] =
    FlatMap(x)(identity)

}
