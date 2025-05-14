package chester.utils.elab

import chester.utils.cell.Cell

sealed trait CellReprAny {}

open trait CellRepr[+A, -B, +C <: Cell[A, B]] extends CellReprAny {
  def tag: String = Integer.toHexString(hashCode)

  override final def toString: String = s"CellId@$tag"
}

type CellReprOf[A, B] = CellRepr[A, B, Cell[A, B]]
type CellReprOfRW[T] = CellRepr[T, T, Cell[T, T]]
type CellReprOfAny = CellRepr[Any, Nothing, Cell[Any, Nothing]]
type CellReprOfR[+T] = CellRepr[T, Nothing, Cell[T, Nothing]]
type CellReprOfW[-T] = CellRepr[Any, T, Cell[Any, T]]
type CellReprOfRWOr[A] = CellReprOf[A, A] | A
