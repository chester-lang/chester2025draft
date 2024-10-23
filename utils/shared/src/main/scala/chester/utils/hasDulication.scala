package chester.utils

extension [T](x: Seq[T]) {
  def hasDuplication: Boolean = x.size != x.distinct.size
}
