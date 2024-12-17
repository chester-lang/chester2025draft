package chester.syntax.core
import chester.uniqid.*
import upickle.default.*

case class Judge(wellTyped: Term, ty: Term, effects: Effects = NoEffect) extends ContainsUniqid derives ReadWriter {
  def substitute(from: TermWithUniqid, to: Term): Judge = Judge(
    wellTyped.substitute(from, to),
    ty.substitute(from, to),
    effects.substitute(from, to).asInstanceOf[Effects]
  )

  def collectMeta: Vector[MetaTerm] =
    wellTyped.collectMeta ++ ty.collectMeta ++ effects.collectMeta
  def replaceMeta(f: MetaTerm => Term): Judge =
    Judge(wellTyped.replaceMeta(f), ty.replaceMeta(f), effects.replaceMeta(f))

  override def collectU(collector: UCollector): Unit = {
    wellTyped.collectU(collector)
    ty.collectU(collector)
    effects.collectU(collector)
  }

  override def replaceU(reranger: UReplacer): Judge = {
    copy(
      wellTyped.replaceU(reranger),
      ty.replaceU(reranger),
      effects.replaceU(reranger).asInstanceOf[Effects]
    )
  }
}
