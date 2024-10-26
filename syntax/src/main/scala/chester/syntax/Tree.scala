package chester.syntax

import chester.syntax.core.Term
import chester.utils.reuse

trait Tree {
  type RootTree <: Tree
  type ThisTree <: Tree

  // this utility method is not that type safe
  protected final inline def thisOr[T <: RootTree](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: RootTree => RootTree, g: TreeMap[RootTree]): RootTree
}

/** means not changing the subtype of Term */
trait TreeMap[-Tre <: Tree] {

  /** note that special rules might apply when x is MetaTerm, which is hard to represent in scala type system */
  def use[T <: Tre](x: T): x.ThisTree
  final inline def apply[T <: Tre](x: T): T = use(x).asInstanceOf[T]
}

implicit inline def convertSpecialMap[T <: Term](inline f: TreeMap[Term]): T => T = x => f.use(x).asInstanceOf[T]
