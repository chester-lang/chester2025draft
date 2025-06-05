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
    [T <: A] => (x: T) => x.descent(f)
  )

  final def descent2(f: TreeMap[A]): ThisTree = descent(
    f(_),
    f
  )

  final def descentRec(f: A => A): A = thisOr {
    f(descent(_.descentRec(f)))
  }
  final def descent2Rec(f: TreeMap[A]): ThisTree = thisOr {
    f(descent2([T <: A] => (x: T) => x.descent2Rec(f))).asInstanceOf[A & ThisTree]
  }

  def inspect(f: A => Unit): Unit = {
    val _ = descent2 {
      [T <: A] =>
        (x: T) => {
          f(x); x.asInstanceOf[x.ThisTree]
      }
    }
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

type TreeMap[Tre <: Tree[Tre]] = [T <: Tre] => (x: T) => x.ThisTree
