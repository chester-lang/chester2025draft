package chester.syntax.core
import chester.uniqid.*
import upickle.default.*

case class Judge(wellTyped: Term, ty: Term, effects: Effects = NoEffect) extends ContainsUniqId derives ReadWriter {
  def substitute(from: TermWithUniqId, to: Term): Judge = Judge(
    wellTyped.substitute(from, to),
    ty.substitute(from, to),
    effects.substitute(from, to).asInstanceOf[Effects]
  )

  def collectMeta: Vector[MetaTerm] =
    wellTyped.collectMeta ++ ty.collectMeta ++ effects.collectMeta
  def replaceMeta(f: MetaTerm => Term): Judge =
    Judge(wellTyped.replaceMeta(f), ty.replaceMeta(f), effects.replaceMeta(f))

  override def collectU(collector: CollectorU): Unit = {
    wellTyped.collectU(collector)
    ty.collectU(collector)
    effects.collectU(collector)
  }

  override def rerangeU(reranger: RerangerU): Judge = {
    copy(
      wellTyped.rerangeU(reranger),
      ty.rerangeU(reranger),
      effects.rerangeU(reranger).asInstanceOf[Effects]
    )
  }
}
