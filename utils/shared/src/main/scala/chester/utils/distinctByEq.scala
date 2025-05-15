package chester.utils
import scala.collection.Factory
import scala.collection.mutable

extension [A, CC[X] <: Iterable[X]](coll: CC[A])
  def distinctByEq(eq: (A, A) => Boolean)(using Factory[A, CC[A]]): CC[A] =
    val builder = summon[Factory[A, CC[A]]].newBuilder
    val seen = mutable.ListBuffer.empty[A]
    for elem <- coll do
      if !seen.exists(e => eq(e, elem)) then
        seen += elem
        builder += elem
    builder.result()
