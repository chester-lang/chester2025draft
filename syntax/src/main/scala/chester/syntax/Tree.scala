package chester.syntax

import chester.syntax.core.Term
import chester.utils.reuse

trait Tree[RootTree <: Tree[RootTree]] extends Any {
  type ThisTree <: Tree[RootTree]
  // it is too hard to verify following properties in scala type system, so we just assume them
  implicit val ev1: Tree[RootTree] <:< RootTree = implicitly[RootTree =:= RootTree].asInstanceOf[Tree[RootTree] <:< RootTree]
  given ev3[T <: RootTree](using x: T): (x.ThisTree <:< RootTree) = implicitly[RootTree <:< RootTree].asInstanceOf[x.ThisTree <:< RootTree]

  // this utility method is not that type safe
  protected final inline def thisOr[T <: RootTree](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: RootTree => RootTree, g: TreeMap[RootTree]): RootTree
  final def descent(f: RootTree => RootTree): RootTree = descent(
    f,
    new TreeMap[RootTree] {
      def use[T <: RootTree](x: T): x.ThisTree = x.descent(f.asInstanceOf).asInstanceOf[x.ThisTree]
    }
  )
  final def descent2(f: TreeMap[RootTree]): ThisTree = descent(
    { x =>
      implicit val ev33 = ev3(using x)
      f.use(x)
    },
    f
  ).asInstanceOf[ThisTree]

  final def descentRecursive(f: RootTree => RootTree): RootTree = thisOr {
    f(descent { a =>
      a.descentRecursive(f(_))
    })
  }

  def inspect(f: RootTree => Unit): Unit = {
    val _ = descent2(new TreeMap[RootTree] {
      def use[T <: RootTree](x: T): x.ThisTree = { f(x); x.asInstanceOf[x.ThisTree] }
    })
    ()
  }

  def inspectRecursive(operator: RootTree => Unit): Unit = {
    inspect { a =>
      a.inspectRecursive(operator(_))
    }
    operator(this.asInstanceOf[RootTree])
  }

}

/** means not changing the subtype of Term */
trait TreeMap[Tre <: Tree[Tre]] {

  /** note that special rules might apply when x is MetaTerm, which is hard to represent in scala type system */
  def use[T <: Tre](x: T): x.ThisTree
  final inline def apply[T <: Tre](x: T): T = use(x).asInstanceOf[T]
}

implicit  def convertSpecialMap2[A <: Tree[A], T <: A]( f: TreeMap[A]): Tree[A] => T = x => f.use(x.asInstanceOf[T]).asInstanceOf[T]

implicit  def conversion1[A <: Tree[A], T <: Tree[A], U <:T](x:Vector[T]): Vector[U] = x.asInstanceOf[Vector[U]]