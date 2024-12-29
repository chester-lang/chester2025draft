package chester.syntax.core

import chester.syntax.core.spec.spec.*

import scala.language.implicitConversions

def convertToSimple[Term<:TermT[Term]](term: Term): simple.Term = {
  implicit inline def innerConvert[From <: TermT[Term], A <: simple.Term](t: From): A = convertToSimple(t.asInstanceOf[Term]).asInstanceOf[A]
  implicit inline def innerXs[From <: TermT[Term], A <: simple.Term](x: Vector[From]): Vector[A] = x.asInstanceOf[Vector[Term]].map(convertToSimple).asInstanceOf[Vector[A]]
  term match {
    case x: CallingArgTermC[Term] => simple.CallingArgTerm(value = x.value, ty = x.ty, name = x.name, vararg = x.vararg, meta = x.meta)
    case x: CallingC[Term] => simple.Calling(args = x.args,implicitly=x.implicitly, meta = x.meta)
    case _ => ???
  }
}