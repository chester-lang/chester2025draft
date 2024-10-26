package chester.syntax

import chester.syntax.core.Term
import chester.utils.reuse

trait Tree {
  type RootTree <: Tree
  type ThisTree <: Tree // it could cause problems if I write `ThisTree <: RootTree` here
  // it is too hard to verify following properties in scala type system, so we just assume them
  private val ev: ThisTree <:< RootTree = implicitly[RootTree <:< RootTree].asInstanceOf[ThisTree <:< RootTree]
  private given ev2[T <: RootTree](using x: T): (x.RootTree =:= RootTree) = implicitly[RootTree =:= RootTree].asInstanceOf[x.RootTree =:= RootTree]
  private given ev3[T <: RootTree](using x: T): (x.ThisTree <:< RootTree) = implicitly[RootTree <:< RootTree].asInstanceOf[x.ThisTree <:< RootTree]

  // this utility method is not that type safe
  protected final inline def thisOr[T <: RootTree](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: RootTree => RootTree, g: TreeMap[RootTree]): RootTree
  final def descent(f: RootTree => RootTree): RootTree = descent(
    f,
    new TreeMap[RootTree] {
      def use[T <: RootTree](x: T): x.ThisTree = x.descent(f.asInstanceOf[x.RootTree => x.RootTree]).asInstanceOf[x.ThisTree]
    }
  )
  final def descent2(f: TreeMap[RootTree]): ThisTree = descent(
    { x =>
      implicit val ev0: (x.RootTree <:< Tree.this.RootTree) = ev2(using x)
      implicit val ev1: (x.ThisTree <:< Tree.this.RootTree) = ev3(using x)
      f.use(x)
    },
    f
  ).asInstanceOf[ThisTree]

  final def descentRecursive(f: RootTree => RootTree): RootTree = thisOr {
    f(descent { a =>
      implicit val ev0: (a.RootTree =:= Tree.this.RootTree) = ev2(using a)
      implicit val ev1: (Tree.this.RootTree =:= a.RootTree) = ev0.flip
      a.descentRecursive (f(_))
    })
  }

  def inspect(f: RootTree => Unit): Unit = {
    val _ = descent2(new TreeMap[RootTree] {
      def use[T <: RootTree](x: T): x.ThisTree = { f(x); x.asInstanceOf[x.ThisTree] }
    })
    ()
  }

  def inspectRecursive(operator: RootTree => Unit): Unit = {
    inspect{a=>
      implicit val ev0: (a.RootTree =:= Tree.this.RootTree) = ev2(using a)
      implicit val ev1: (Tree.this.RootTree =:= a.RootTree) = ev0.flip
      
      a.inspectRecursive(operator(_))}
    operator(this.asInstanceOf[RootTree])
  }

}

/** means not changing the subtype of Term */
trait TreeMap[-Tre <: Tree] {

  /** note that special rules might apply when x is MetaTerm, which is hard to represent in scala type system */
  def use[T <: Tre](x: T): x.ThisTree
  final inline def apply[T <: Tre](x: T): T = use(x).asInstanceOf[T]
}

implicit inline def convertSpecialMap[T <: Term](inline f: TreeMap[Term]): T => T = x => f.use(x).asInstanceOf[T]
