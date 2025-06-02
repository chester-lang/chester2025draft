package chester.syntax

import chester.utils.reuse

import scala.language.implicitConversions

trait Tree[A <: Tree[A]] extends Any {
  type RootTree = A
  // ThisTree is not specified like in  other types in MetaTerm. ThisType = Term in MetaTerm
  type ThisTree <: Tree[A]
  // it is too hard to express the following properties in scala type system, so we just assume them
  implicit val ev1: Tree[A] <:< A = implicitly[A =:= A].asInstanceOf[Tree[A] <:< A]
  given ev3[T <: A](using x: T): (x.ThisTree <:< A) = implicitly[A <:< A].asInstanceOf[x.ThisTree <:< A]
  implicit inline def thisIsThisTree(_ignored: this.type): ThisTree = this.asInstanceOf[ThisTree]

  // this utility method is not that type safe
  protected final inline def thisOr[T <: A](inline x: T): T =
    reuse(this.asInstanceOf[T], x)

  def descent(f: A => A, g: TreeMap[A]): ThisTree
  final def descent(f: A => A): ThisTree = descent(
    f,
    new TreeMap[A] {
      def use[T <: A](x: T): x.ThisTree = x.descent(f.asInstanceOf).asInstanceOf[x.ThisTree]
    }
  )
  final def descent2(f: TreeMap[A]): ThisTree = descent(
    f.use,
    f
  )

  final def descentRec(f: A => A): A = thisOr {
    f(descent(_.descentRec(f(_))))
  }

  def inspect(f: A => Unit): Unit = {
    val _ = descent2(new TreeMap[A] {
      def use[T <: A](x: T): x.ThisTree = { f(x); x.asInstanceOf[x.ThisTree] }
    })
  }

  final def inspectRec(operator: A => Unit): Unit = {
    inspect(_.inspectRec(operator(_)))
    operator(this.asInstanceOf[A])
  }

  def mapFlatten[B](f: A => Seq[B]): Vector[B] = {
    var result = Vector.empty[B]
    inspectRec(term => result ++= f(term))
    result
  }

}

/** means not changing the subtype of Term, except for MetaTerm */
trait TreeMap[Tre <: Tree[Tre]] {
  def use[T <: Tre](x: T): x.ThisTree
}

implicit def convertSpecialMap2[A <: Tree[A], T <: A](f: TreeMap[A]): Tree[A] => T = x => f.use(x.asInstanceOf[T]).asInstanceOf[T]
