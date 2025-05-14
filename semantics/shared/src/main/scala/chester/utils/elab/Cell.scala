package chester.utils.elab

import chester.utils.cell.CellContent

sealed trait CellReprAny {}

open trait Cell[+A, -B, +C <: CellContent[A, B]] extends CellReprAny {
  def tag: String = Integer.toHexString(hashCode)

  override final def toString: String = s"CellId@$tag"
}

type Repr[A, B] = Cell[A, B, CellContent[A, B]]
type ReprRW[T] = Cell[T, T, CellContent[T, T]]
type ReprAny = Cell[Any, Nothing, CellContent[Any, Nothing]]
type ReprR[+T] = Cell[T, Nothing, CellContent[T, Nothing]]
type ReprW[-T] = Cell[Any, T, CellContent[Any, T]]
type ReprRWOr[A] = Repr[A, A] | A
