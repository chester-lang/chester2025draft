package chester.utils.elab

import chester.utils.cell.Cell

sealed trait CellReprAny {}

open trait CellRepr[+A, -B, +C <: Cell[A, B]] extends CellReprAny {
  def tag: String = Integer.toHexString(hashCode)

  override final def toString: String = s"CellId@$tag"
}

type Repr[A, B] = CellRepr[A, B, Cell[A, B]]
type ReprRW[T] = CellRepr[T, T, Cell[T, T]]
type ReprAny = CellRepr[Any, Nothing, Cell[Any, Nothing]]
type ReprR[+T] = CellRepr[T, Nothing, Cell[T, Nothing]]
type ReprW[-T] = CellRepr[Any, T, Cell[Any, T]]
type ReprRWOr[A] = Repr[A, A] | A
