package chester.utils
import spire.math.Natural

object Nat {
  def apply(n: Int): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
  def apply(n: Long): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")

  // TODO: check if BigInt >= 0 is ok or not?
  def apply(n: BigInt): Natural = if (n >= 0) Natural.apply(n) else throw new IllegalArgumentException("Negative number not allowed")
}
