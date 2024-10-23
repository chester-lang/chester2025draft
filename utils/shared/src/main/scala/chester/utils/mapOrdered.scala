package chester.utils

extension [T](xs: Vector[T]) {

  /** when f has side effect and annotate that we needs order */
  inline def mapOrdered[U](inline f: T => U): Vector[U] = xs.map(f)
  inline def flatMapOrdered[U](inline f: T => IterableOnce[U]): Vector[U] =
    xs.flatMap(f)
}
