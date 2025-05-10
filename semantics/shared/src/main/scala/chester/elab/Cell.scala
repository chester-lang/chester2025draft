package chester.elab

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
